export type Image = HTMLImageElement;

let imageMap = new Map<string, Image>();

export const loadAsset = (src: string): Image => {
  if (imageMap.has(src)) return imageMap.get(src)!;

  const result = new Image();
  result.src = src;

  result.onload = () => {
    result.height = result.naturalHeight;
    result.width = result.naturalWidth;
  };

  imageMap.set(src, result);

  return result;
};
