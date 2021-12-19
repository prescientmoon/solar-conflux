import { useRef, useEffect } from "preact/hooks";

export type CanvasProps = {
  emitRef: (ref: HTMLCanvasElement) => void;
};

export const Canvas = (props: CanvasProps) => {
  const ref = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    if (ref.current) props.emitRef(ref.current);
  }, [ref]);

  return <canvas id="game-canvas" ref={ref}></canvas>;
};
