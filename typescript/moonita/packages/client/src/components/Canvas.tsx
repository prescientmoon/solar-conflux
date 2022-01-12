import { useRef, useEffect } from "preact/hooks";

export type CanvasProps = {
  emitRef: (ref: HTMLCanvasElement) => void;
  className: string;
};

export const Canvas = (props: CanvasProps) => {
  const ref = useRef<HTMLCanvasElement>(null);

  useEffect(() => {
    if (ref.current) props.emitRef(ref.current);
  }, [ref]);

  return (
    <canvas
      className={[props.className, "game__game--canvas"].join(" ")}
      ref={ref}
    ></canvas>
  );
};
