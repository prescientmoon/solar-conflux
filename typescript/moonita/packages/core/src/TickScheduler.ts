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

export class TikScheduler<T> {
  private tasks = new Map<number, Task<T>[]>();
  private nextTaskId = 0;
  private idToTask = new Map<TaskId, TaskLocation>();

  private ensureTaskArrayExists(tick: number) {
    if (!this.tasks.has(tick)) this.tasks.set(tick, []);
  }

  public constructor() {}
  public schedule(
    tick: number,
    task: T,
    repetitionInterval: number | null = null
  ) {
    const id = this.nextTaskId++;
    this.ensureTaskArrayExists(tick);
    this.tasks.get(tick)!.push({
      task,
      id,
      repetitionInterval,
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
      }
    }

    this.tasks.delete(tick);
  }
}
