package de.lolhens.minecraft.blockshifter

import de.lolhens.minecraft.blockshifter.block.RailBlock
import de.lolhens.minecraft.blockshifter.util.EntityMover
import net.fabricmc.api.ModInitializer
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents
import net.fabricmc.loader.api.FabricLoader
import net.fabricmc.loader.api.metadata.ModMetadata
import net.minecraft.item.{BlockItem, Item, ItemGroup}
import net.minecraft.util.Identifier
import net.minecraft.util.registry.Registry

import scala.jdk.CollectionConverters._

object BlockshifterMod extends ModInitializer {
  val metadata: ModMetadata = {
    FabricLoader.getInstance().getEntrypointContainers("main", classOf[ModInitializer])
      .iterator().asScala.find(this eq _.getEntrypoint).get.getProvider.getMetadata
  }

  val railBlockId = new Identifier(metadata.getId, "rail")
  val railBlock: RailBlock = new RailBlock()

  override def onInitialize(): Unit = {
    Registry.register(Registry.BLOCK, railBlockId, railBlock)
    Registry.register(Registry.ITEM, railBlockId, new BlockItem(railBlock, new Item.Settings().group(ItemGroup.BUILDING_BLOCKS)))

    ServerTickEvents.START_WORLD_TICK.register { world =>
      EntityMover(world).moveAll()
    }
  }
}