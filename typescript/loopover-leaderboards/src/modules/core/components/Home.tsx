import { Block, Col } from "jsxstyle";
import { h } from "preact";
import { Centered } from "./Centered";
import { Layer, Stack } from "./Stack";

const angle = -4;
const tan = -Math.tan((angle / 180) * Math.PI);
const offset = `${tan} * 50vw`;
const offsetCalc = `calc(${offset})`;

export const Home = () => {
    return (
        <Stack height="100%" background="var(--primary)">
            <Layer
                transformOrigin="top left"
                transform={`skewY(${angle}deg)`}
                height={`calc(50% + ${offset})`}
                filter="blur(4px) brightness(0.8) sepia(60%) grayscale(30%)"
                background-image="url(../../../assets/board.png)"
            />
            <Layer>
                <Centered height="100%">
                    <Centered
                        height="50%"
                        color="white"
                        fontSize="4rem"
                        fontFamily="var(--title-font)"
                        marginBottom={offsetCalc}
                    >
                        <Block>Loopover</Block>
                    </Centered>
                    <Centered
                        height={`calc(50vh - ${offset})`}
                        fontSize="30px"
                        color="var(--on-primary)"
                        maxWidth="var(--max-width)"
                        margin={offsetCalc}
                    >
                        <Block>
                            Loopover is a 2D Rubik's Cube-like puzzle originally
                            developed by Cary Huang
                        </Block>
                    </Centered>
                </Centered>
            </Layer>
        </Stack>
    );
};
