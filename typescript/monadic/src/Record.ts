export const mapRecord = <K extends string | number, V, W>(
  record: Record<K, V>,
  mapper: (v: V, k: K) => W
): Record<K, W> => {
  return Object.fromEntries(
    Object.entries(record).map(([key, value]) => [
      key,
      mapper(value as V, key as K),
    ])
  ) as any;
};
