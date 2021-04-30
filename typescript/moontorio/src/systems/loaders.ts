import { ItemComponents, Loader } from "../gameState";

export const implBeltForLoader: ItemComponents<Loader>["beltLike"] = {
  push() {
    console.log("pushed into loader");
  },
};
