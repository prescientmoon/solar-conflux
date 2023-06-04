// @ts-check
const ecs = require("@thi.ng/ecs");

exports.makeEcs =
  ({ size, components }) =>
  () => {
    const thisEcs = new ecs.ECS({
      capacity: size,
    });

    for (const key in components) {
      const component = { id: key };
      Object.assign(component, components[key]);
      thisEcs.defComponent(component);
    }

    return ecs;
  };

exports.unsafeCreateEntity = (components, /** @type {ecs.ECS} */ ecs) => {
  return ecs.defEntity(components);
};

exports.deleteEntity =
  (/** @type {number} */ id) => (/** @type {ecs.ECS} */ ecs) => () => {
    return ecs.deleteID(id);
  };

exports.unsafeCreateGroup = (
  components,
  owned,
  opts,
  /** @type {ecs.ECS} */ ecs
) => {
  return ecs.defGroup(components, owned, opts);
};

exports.unsafeGetComponentManager = (
  /** @type {string} */ name,
  /** @type {ecs.ECS} */ ecs
) => {
  return ecs.components.get(name);
};
