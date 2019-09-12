import { PersistentSubject } from "rxjs-extra"
import { BehaviorSubject } from "rxjs"

export class Settings<T extends Record<string, unknown>> {
  public settingMap = new Map<keyof T, PersistentSubject<any>>()

  public constructor(public defaults?: T) {
    if (defaults) {
      for (const key in defaults) {
        this.set(key, defaults[key])
      }
    }
  }

  public set<K extends (keyof T) & string>(name: K, defaultValue: T[K]) {
    if (process.browser) {
      this.settingMap.set(name, new PersistentSubject(name, defaultValue))
    }
  }

  public get<K extends (keyof T) & string>(
    name: K,
    fallback: T[K] = this.defaults![name]
  ) {
    if (process.browser) {
      return this.settingMap.get(name) as PersistentSubject<T[K]>
    }

    return new BehaviorSubject(fallback)
  }
}
