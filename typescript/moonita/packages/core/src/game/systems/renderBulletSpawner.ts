import { State } from "../State";

export const renderBulletSpawners = (state: State) => {
  state.ctx.fillStyle = "black";

  state.queries.bulletEmitters._forEach((eid) => {
    state.ctx.fillRect(
      state.components.transform.position.x[eid] - 20,
      state.components.transform.position.y[eid] - 20,
      40,
      40
    );
  });
};

export const renderBullets = (state: State) => {
  state.ctx.fillStyle = "red";
  state.ctx.strokeStyle = "none";

  state.queries.bullets._forEach((eid) => {
    state.ctx.fillRect(
      state.components.transform.position.x[eid] - 10,
      state.components.transform.position.y[eid] - 10,
      20,
      20
    );
  });
};
