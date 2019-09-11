export const createOptionKeyGetter = (brushName: string) => (option: string) =>
  `${brushName}-${option}`
