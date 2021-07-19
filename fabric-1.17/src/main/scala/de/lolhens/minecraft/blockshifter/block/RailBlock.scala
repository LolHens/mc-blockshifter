package de.lolhens.minecraft.blockshifter.block

import de.lolhens.minecraft.blockshifter.util.{EntityMover, WorldUtil}
import net.fabricmc.fabric.api.`object`.builder.v1.block.FabricBlockSettings
import net.minecraft.block._
import net.minecraft.block.entity.BlockEntity
import net.minecraft.block.piston.PistonBehavior
import net.minecraft.entity.Entity
import net.minecraft.item.ItemPlacementContext
import net.minecraft.server.world.ServerWorld
import net.minecraft.state.StateManager
import net.minecraft.state.property.BooleanProperty
import net.minecraft.util.math.{BlockPos, Box, Direction, Vec3d}
import net.minecraft.util.{BlockMirror, BlockRotation}
import net.minecraft.world.World

import scala.jdk.CollectionConverters._
import scala.util.chaining._

class RailBlock() extends FacingBlock(RailBlock.settings) {
  def getState(facing: Direction, rotated: Boolean): BlockState =
    getStateManager.getDefaultState
      .`with`(FacingBlock.FACING, facing)
      .`with`(RailBlock.ROTATED, java.lang.Boolean.valueOf(rotated))
      .`with`(RailBlock.POWERED, java.lang.Boolean.valueOf(false))

  setDefaultState(getState(facing = Direction.UP, rotated = false))

  override protected def appendProperties(stateManager: StateManager.Builder[Block, BlockState]): Unit =
    stateManager.add(FacingBlock.FACING, RailBlock.ROTATED, RailBlock.POWERED)

  override def rotate(state: BlockState, rotation: BlockRotation): BlockState =
    state.`with`(FacingBlock.FACING, rotation.rotate(state.get(FacingBlock.FACING)))

  override def mirror(state: BlockState, mirror: BlockMirror): BlockState =
    state.rotate(mirror.getRotation(state.get(FacingBlock.FACING)))

  private val preferredAxes: Array[Direction.Axis] =
    Array(Direction.Axis.Y, Direction.Axis.X, Direction.Axis.Z)

  private def preferredMovementAxisFromSurrounding(world: World,
                                                   pos: BlockPos,
                                                   facing: Direction,
                                                   preferredAxis: Option[Direction.Axis],
                                                   rotateSurrounding: Boolean = true): Option[Direction.Axis] = {
    val facingAxis = facing.getAxis
    val connectedAxes = Direction.values
      .iterator
      .filterNot(_.getAxis == facingAxis)
      .filter { direction =>
        val offset: BlockPos = pos.offset(direction)
        val state = world.getBlockState(offset)
        state.isOf(this) && (state.getBlock match {
          case rail: RailBlock =>
            val otherFacing = state.get(FacingBlock.FACING)
            otherFacing == facing && {
              val otherRotated = state.get(RailBlock.ROTATED)
              val otherMovementAxis = movementAxis(otherFacing, otherRotated)
              otherMovementAxis == direction.getAxis || (rotateSurrounding && {
                rail.preferredMovementAxisFromSurrounding(
                  world,
                  offset,
                  otherFacing,
                  Some(otherMovementAxis),
                  rotateSurrounding = false
                )
                  .forall(_ == direction.getAxis)
              })
            }
        })
      }
      .map(_.getAxis)
      .toArray

    preferredAxis.filter(connectedAxes.contains)
      .orElse(connectedAxes.headOption)
  }

  private def movementAxisFromSurrounding(world: World,
                                          pos: BlockPos,
                                          facing: Direction,
                                          preferredAxis: Option[Direction.Axis]): Direction.Axis = {
    val facingAxis = facing.getAxis
    preferredMovementAxisFromSurrounding(world, pos, facing, preferredAxis)
      .getOrElse {
        val axes = preferredAxes
          .iterator
          .filterNot(_ == facingAxis)
          .toArray

        preferredAxis.filter(axes.contains)
          .getOrElse(axes.head)
      }
  }

  private def isRotatedFromSurrounding(world: World, pos: BlockPos, facing: Direction, preferRotated: Option[Boolean]): Boolean = {
    val preferredAxis = preferRotated.map(movementAxis(facing, _))
    val axis = movementAxisFromSurrounding(world, pos, facing, preferredAxis)
    preferredAxes.iterator.filterNot(_ == facing.getAxis).indexOf(axis) > 0
  }

  override def getPlacementState(ctx: ItemPlacementContext): BlockState = {
    val facing: Direction = ctx.getPlayerLookDirection.getOpposite
    val rotated = isRotatedFromSurrounding(ctx.getWorld, ctx.getBlockPos, facing, None)
    getState(facing = facing, rotated = rotated)
  }


  override def neighborUpdate(state: BlockState, world: World, pos: BlockPos, block: Block, fromPos: BlockPos, notify: Boolean): Unit = {
    var newState: BlockState = state

    if (block == this || world.getBlockState(fromPos).isOf(this))
      newState.tap { state =>
        val facing = state.get(FacingBlock.FACING)
        val rotated = state.get(RailBlock.ROTATED)
        val newRotated = isRotatedFromSurrounding(world, pos, facing, Some(rotated))
        if (newRotated != rotated) {
          newState = state.`with`(RailBlock.ROTATED, java.lang.Boolean.valueOf(newRotated))
          world.setBlockState(pos, newState, 3)
        }
      }

    val isPowered = world.isReceivingRedstonePower(pos)
    if (isPowered != state.get(RailBlock.POWERED))
      newState.tap { state =>
        newState = state.`with`(RailBlock.POWERED, java.lang.Boolean.valueOf(isPowered))
        world.setBlockState(pos, newState, 2)

        if (isPowered) {
          world match {
            case serverWorld: ServerWorld =>
              shiftBlocks(serverWorld, pos, newState)

            case _ =>
          }
        }
      }
  }

  def movementDirectionFromBlockState(state: BlockState): Direction = {
    val facing = state.get(FacingBlock.FACING)
    val rotated = state.get(RailBlock.ROTATED)
    movementDirection(facing, rotated)
  }

  def movementAxis(facing: Direction, rotated: Boolean): Direction.Axis =
    preferredAxes.iterator.filterNot(_ == facing.getAxis).drop(if (rotated) 1 else 0).next()

  def movementDirection(facing: Direction, rotated: Boolean): Direction = {
    val opposite = Direction.values.iterator.filter(_.getAxis == facing.getAxis).indexOf(facing) > 0
    val axis = movementAxis(facing, rotated)
    Direction.values.iterator.filter(_.getAxis == axis).drop(if (opposite) 1 else 0).next()
  }

  private def shiftBlocks(world: ServerWorld, pos: BlockPos, state: BlockState, reverse: Boolean = false): Boolean = {
    val facing = state.get(FacingBlock.FACING)
    val rotated = state.get(RailBlock.ROTATED)
    val direction = movementDirection(facing, rotated).pipe(e => if (reverse) e.getOpposite else e)
    shiftBlocks(world, pos, facing, direction)
  }

  private val air = Blocks.AIR.getDefaultState

  private def shiftBlocks(world: ServerWorld, pos: BlockPos, facing: Direction, movementDirection: Direction): Boolean = {
    def follow(start: BlockPos, direction: Direction): Iterator[BlockPos] =
      Iterator.iterate(start)(_.offset(direction))

    def isRail(pos: BlockPos, other: Boolean): Boolean = {
      val state = world.getBlockState(pos)
      state.isOf(this) && {
        val railFacing = state.get(FacingBlock.FACING)
        railFacing == (if (other) facing.getOpposite else facing) &&
          movementDirectionFromBlockState(state).getAxis == movementDirection.getAxis
      }
    }

    def isThisRail(pos: BlockPos): Boolean = isRail(pos, other = false)

    def isOtherRail(pos: BlockPos): Boolean = isRail(pos, other = true)

    val neighborAlreadyPowered =
      List(movementDirection, movementDirection.getOpposite)
        .iterator
        .map[BlockPos](pos.offset)
        .filter(isThisRail)
        .exists(world.getBlockState(_).get(RailBlock.POWERED))

    if (!neighborAlreadyPowered) {
      val thisRailStart = follow(pos, movementDirection.getOpposite).takeWhile(isThisRail).toList.last
      val thisRailLength = follow(thisRailStart, movementDirection).takeWhile(isThisRail).size

      //println("this rail length: " + thisRailLength)
      //println("facing: " + facing)
      //println("move direction: " + movementDirection)

      follow(thisRailStart, movementDirection)
        .take(thisRailLength)
        .zipWithIndex
        .flatMap {
          case (railStart, railStartOffset) =>
            follow(railStart, facing)
              .take(RailBlock.maxRailDistance)
              .zipWithIndex
              .drop(1)
              .scanLeft[(Int, Option[(BlockPos, Int)])]((0, None)) {
                case (nop@(recursion, _), otherRail@(otherRailPos, _)) =>
                  if (isOtherRail(otherRailPos)) {
                    if (recursion <= 0)
                      (0, Some(otherRail))
                    else
                      (recursion - 1, None)
                  } else if (isThisRail(otherRailPos)) {
                    (recursion + 1, None)
                  } else {
                    nop
                  }
              }
              .collectFirst(Function.unlift(_._2))
              .map {
                case (otherRailPos, railDistance) =>
                  (railStart, railStartOffset, otherRailPos, railDistance)
              }
        }
        .nextOption()
        .foreach {
          case (railStart, railStartOffset, otherRailPos, railDistance) =>
            val otherRailLength = follow(otherRailPos, movementDirection).takeWhile(isOtherRail).size
            val railLength = Math.min(thisRailLength - railStartOffset, otherRailLength)

            //println("distance: " + railDistance)
            //println("length: " + railLength)

            val alreadyPowered =
              (follow(railStart, movementDirection).take(railLength) ++
                follow(otherRailPos, movementDirection).take(railLength))
                .filterNot(_ == pos)
                .exists(world.getBlockState(_).get(RailBlock.POWERED))

            if (!alreadyPowered) {
              def betweenRails(posOnRail: BlockPos): Iterator[BlockPos] =
                follow(posOnRail.offset(facing), facing).take(railDistance - 1)

              def isEmpty(pos: BlockPos, state: BlockState): Boolean =
                state.isAir || (state.getPistonBehavior match {
                  case PistonBehavior.IGNORE | PistonBehavior.DESTROY => true
                  case _ => false
                })

              def isMovable(pos: BlockPos, state: BlockState): Boolean =
                PistonBlock.isMovable(state, world, pos, movementDirection, true, movementDirection)

              def areAllEmpty(iterator: IterableOnce[(BlockPos, BlockState)]): Boolean =
                iterator.iterator.forall(e => isEmpty(e._1, e._2))

              def areAllMovable(iterator: IterableOnce[(BlockPos, BlockState)]): Boolean =
                iterator.iterator.forall(e => isMovable(e._1, e._2))

              def isRow(posOnRail: BlockPos, f: Seq[(BlockPos, BlockState)] => Boolean): Boolean =
                f(betweenRails(posOnRail).map(pos => (pos, world.getBlockState(pos))).toSeq)

              def isRowEmptyOrImmovable(posOnRail: BlockPos): Boolean =
                isRow(posOnRail, blocks => areAllEmpty(blocks) || !areAllMovable(blocks))

              def clearFirstRow(posOnRail: BlockPos): Boolean = {
                val row = betweenRails(posOnRail).map(pos => (pos, world.getBlockState(pos))).toSeq
                val rowEmpty = areAllEmpty(row)
                if (rowEmpty) row.foreach {
                  case (pos, state) => if (!state.isAir && state.getPistonBehavior == PistonBehavior.DESTROY) {
                    val blockEntity = if (state.hasBlockEntity) world.getBlockEntity(pos) else null
                    Block.dropStacks(state, world, pos, blockEntity)
                  }
                }
                rowEmpty
              }

              val emptyRowsStart =
                follow(railStart, movementDirection)
                  .take(railLength)
                  .takeWhile(isRowEmptyOrImmovable)
                  .size

              if (emptyRowsStart < railLength - 1) {
                val railEnd: BlockPos = railStart.offset(movementDirection, railLength - 1)

                val emptyRowsEnd =
                  follow(railEnd, movementDirection.getOpposite)
                    .take(railLength)
                    .takeWhile(isRowEmptyOrImmovable)
                    .size

                val overhangStart =
                  if (emptyRowsStart > 0) -emptyRowsStart
                  else
                    follow(railStart.offset(movementDirection.getOpposite), movementDirection.getOpposite)
                      .take(emptyRowsEnd)
                      .takeWhile(!isRowEmptyOrImmovable(_))
                      .size

                val overhangEnd =
                  if (emptyRowsEnd > 0) -emptyRowsEnd
                  else
                    follow(railEnd.offset(movementDirection), movementDirection)
                      .take(emptyRowsStart)
                      .takeWhile(!isRowEmptyOrImmovable(_))
                      .size

                //println("overhang start: " + overhangStart)
                //println("overhang end: " + overhangEnd)

                val shiftStart: BlockPos = railStart.offset(movementDirection, -overhangStart)
                val shiftLength: Int = railLength + overhangStart + overhangEnd
                val shiftEnd: BlockPos = shiftStart.offset(movementDirection, shiftLength - 1)

                if (clearFirstRow(shiftEnd.offset(movementDirection))) {
                  //println("shift start: " + shiftStart)
                  //println("shift end: " + shiftStart)

                  val allRowsMovable =
                    follow(shiftEnd, movementDirection.getOpposite)
                      .take(shiftLength)
                      .forall(isRow(_, areAllMovable))

                  if (allRowsMovable) {
                    val stateList: Seq[(BlockPos, BlockPos, BlockState, Option[BlockEntity])] =
                      follow(shiftEnd, movementDirection.getOpposite)
                        .take(shiftLength)
                        .flatMap(betweenRails(_).map { pos =>
                          val offset: BlockPos = pos.offset(movementDirection)
                          val state: BlockState = world.getBlockState(pos)
                          val entityOption: Option[BlockEntity] = Option(world.getBlockEntity(pos))
                          (pos, offset, state, entityOption)
                        })
                        .toSeq

                    stateList.foreach {
                      case (pos, offset, state, entityOption) =>
                        val flags = 2 | 16 | 32 | 64
                        world.removeBlockEntity(pos)
                        world.setBlockState(pos, air, flags)
                        entityOption.foreach(_.cancelRemoval())
                        WorldUtil.setBlockStateWithBlockEntity(world, offset, state, entityOption.orNull, flags)
                    }

                    def updateBlockAndNeighbors(pos: BlockPos, state: BlockState, extraFlags: Int = 0): Unit = {
                      val flags = (1 | 2 | extraFlags) & -34
                      world.updateNeighbor(pos, state.getBlock, pos)
                      state.updateNeighbors(world, pos, flags)
                      state.prepare(world, pos, flags)
                    }

                    stateList.foreach {
                      case (_, offset, state, _) =>
                        val newState = Block.postProcessState(state, world, offset)
                        Block.replace(state, newState, world, offset, 2 | 16 | 64)
                        updateBlockAndNeighbors(offset, newState, extraFlags = 64)
                    }

                    betweenRails(shiftStart).foreach { pos =>
                      updateBlockAndNeighbors(pos, air, extraFlags = 64)
                    }

                    val box: Box = {
                      val a: BlockPos = shiftStart.offset(facing)
                      val b: BlockPos = shiftEnd.offset(facing, railDistance - 1)

                      val minX = Math.min(a.getX, b.getX)
                      val minY = Math.min(a.getY, b.getY)
                      val minZ = Math.min(a.getZ, b.getZ)
                      val maxX = Math.max(a.getX, b.getX) + 1
                      val maxY = Math.max(a.getY, b.getY) + 1.01
                      val maxZ = Math.max(a.getZ, b.getZ) + 1

                      new Box(minX, minY, minZ, maxX, maxY, maxZ)
                    }

                    val movementVector: Vec3d = Vec3d.of(movementDirection.getVector)
                    world.getNonSpectatingEntities(classOf[Entity], box).iterator.asScala.foreach { entity =>
                      /*val newPos = entity.getPos.add(movementVector)
                      entity.teleport(newPos.getX, newPos.getY, newPos.getZ)*/
                      EntityMover(world).queueMove(entity, movementVector)
                    }

                    return true
                  }
                }
              }
            }
        }
    }

    false
  }
}

object RailBlock {
  private val settings =
    FabricBlockSettings
      .of(Material.STONE)
      .hardness(2.0F)

  val ROTATED: BooleanProperty = BooleanProperty.of("rotated")
  val POWERED: BooleanProperty = BooleanProperty.of("powered")

  val maxRailDistance: Int = 64
}
