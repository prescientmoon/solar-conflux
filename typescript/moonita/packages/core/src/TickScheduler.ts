import { CircularBuffer } from "./CircularBuffer";

export type TaskId = number;
export interface Task<T> {
  task: T;
  repetitionInterval: number | null;
  id: TaskId;
}

export interface TaskLocation {
  tick: number;
  index: number;
}

/** A general purpouse event emitter. */
export class TickScheduler<T> {
  private tasks = new Map<number, Task<T>[]>();
  private nextTaskId = 0;
  private idToTask = new Map<TaskId, TaskLocation>();

  private ensureTaskArrayExists(tick: number) {
    if (!this.tasks.has(tick)) this.tasks.set(tick, []);
  }

  public constructor() {}

  /** An event is a task which has to manually get triggered. */
  public scheduleEvent(tick: number, task: T) {
    return this.schedule(-tick, task, null);
  }

  /** Manually trigger an event, returning all the tasks we have to handle */
  public triggerEvent(id: number) {
    const tasks = this.tasks.get(-id);

    if (tasks === undefined) return [];

    this.handleTick(id);

    return tasks;
  }

  public schedule(
    tick: number,
    task: T,
    repetitionInterval: number | null = null
  ) {
    const id = this.nextTaskId++;
    this.ensureTaskArrayExists(tick);
    const length = this.tasks.get(tick)!.push({
      task,
      id,
      repetitionInterval,
    });

    this.idToTask.set(id, {
      index: length - 1,
      tick,
    });

    return id;
  }

  public unschedule(id: TaskId) {
    const location = this.idToTask.get(id)!;

    const tasks = this.tasks.get(location.tick)!;

    // TODO: abstract this away
    tasks[location.index] = tasks[tasks.length - 1];
    tasks.pop();

    if (tasks.length)
      this.idToTask.get(tasks[location.index].id)!.index = location.index;

    this.idToTask.delete(id);
  }

  public getTasks(tick: number): Task<T>[] {
    if (!this.tasks.has(tick)) return [];

    return this.tasks.get(tick)!;
  }

  public handleTick(tick: number) {
    const tasks = this.tasks.get(tick);
    if (!tasks) return;

    for (const task of tasks) {
      if (task.repetitionInterval !== null) {
        const next = tick + task.repetitionInterval;

        this.ensureTaskArrayExists(next);

        const tasks = this.tasks.get(next)!;
        const length = tasks.push(task);
        const location = this.idToTask.get(task.id)!;

        location.tick = next;
        location.index = length - 1;
      } else {
        this.idToTask.delete(task.id);
      }
    }

    this.tasks.delete(tick);
  }
}
