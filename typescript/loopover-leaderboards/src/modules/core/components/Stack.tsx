import { Block, CSSProperties } from "jsxstyle";
import { FunctionComponent, h } from "preact";

export const Stack: FunctionComponent<CSSProperties> = props => {
    return <Block {...props} position="relative" />;
};

export const Layer: FunctionComponent<CSSProperties> = props => {
    return (
        <Block
            top={0}
            left={0}
            bottom={0}
            right={0}
            position="absolute"
            width="100%"
            height="100%"
            {...props}
        />
    );
};
