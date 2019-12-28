import { Col, CSSProperties } from "jsxstyle";
import { FunctionComponent, h } from "preact";

export const Centered: FunctionComponent<CSSProperties> = props => {
    return <Col justifyContent="center" alignItems="center" {...props} />;
};
