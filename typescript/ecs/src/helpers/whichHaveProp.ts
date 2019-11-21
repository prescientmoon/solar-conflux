export const withProp = <T extends object, K extends keyof T>(
  items: T[],
  property: K
) => items.filter(item => !!item[property]) as Array<T & Required<Pick<T, K>>>
