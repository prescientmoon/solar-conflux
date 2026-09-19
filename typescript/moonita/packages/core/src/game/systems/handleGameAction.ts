import { Task, TaskId } from "../../TickScheduler";
import { Flag } from "../common/Flags";
import {
  actionIdMask,
  GameAction,
  maxActionCount,
  onDespawn,
} from "../GameAction";
import { SimulationState, stateIsComplete } from "../State";
import { castWand } from "./interpretWands";
import { clearSprite } from "./renderTextures";
import { spawnBullets } from "./spawnBullet";

export function handleGameAction(state: SimulationState, task: Task<number>) {
  const action = task.task;
  const id = action & actionIdMask;

  const payload = action >> maxActionCount;

  if (state.flags[Flag.DebugEventLogs])
    console.log(
      // @ts-ignore enum indexing
      `${GameAction[id]} ${payload}`
    );

  if (id === GameAction.DespawnEntity) {
    triggerEvent(state, onDespawn(payload));
    state.ecs.destroyEntity(payload);
  } else if (id === GameAction.HandleBulletSpawner)
    spawnBullets(state, payload);
  else if (id === GameAction.DebugLog) {
    console.log("Debug log!");
  } else if (id === GameAction.ClearSprite) {
    if (stateIsComplete(state)) clearSprite(state, payload);
    else throw new Error("Impossible"); // TODO: check this at compile time
  } else if (id === GameAction.ShootWand) {
    castWand(state, payload);
  }
}

export function triggerEvent(state: SimulationState, event: TaskId) {
  const tasks = state.tickScheduler.triggerEvent(event);

  for (let i = 0; i < tasks.length; i++) {
    const task = tasks[i];

    handleGameAction(state, task);
  }
}
