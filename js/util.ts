export function extVal(
  data: { [k: string]: any },
  lookup: string | null | undefined
): any {
  return lookup && data[lookup];
}
