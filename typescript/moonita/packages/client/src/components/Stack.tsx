import { ComponentChildren } from "preact";
import "../styles/layer.css";

// ========== Types
export interface StackProps {
  children: ComponentChildren;
  className?: string;
}

export type LayerProps = StackProps;

// ========== Components
export const Stack = ({ children, className }: StackProps) => {
  return <div className={[className, "stack"].join(" ")}>{children}</div>;
};

export const Layer = ({ children }: LayerProps) => {
  return <div className="stack__stack--layer">{children}</div>;
};
