import { clamp, normalizeAngle, toDirectionalAngle } from "../math";
import { AxisDirection, getAxisDirection } from "./common/Axis";
import { Thruster, ThrusterConfiguration } from "./State";

export const enum VanillaThrusterConfigurationId {
  SingleThruster,
  DualThruster,
}

const configurations: Record<
  VanillaThrusterConfigurationId,
  ThrusterConfiguration
> = {
  [VanillaThrusterConfigurationId.SingleThruster]: {
    thrusters: [
      {
        angle: Math.PI,
        strength: 1,
        position: {
          angle: Math.PI,
          radius: 1,
        },
      },
    ],
  },
  [VanillaThrusterConfigurationId.DualThruster]: {
    thrusters: [
      {
        angle: (2 * Math.PI) / 3,
        strength: 1,
        position: {
          angle: (2 * Math.PI) / 3,
          radius: 1,
        },
      },
      {
        angle: (4 * Math.PI) / 3,
        strength: 1,
        position: {
          angle: (4 * Math.PI) / 3,
          radius: 1,
        },
      },
    ],
  },
};

export const thrusterConfiguration: ReadonlyArray<ThrusterConfiguration> =
  Object.entries(configurations).reduce(
    (prev: Array<ThrusterConfiguration>, [id, config]: [string, any]) => {
      (prev as any)[id] = config;

      return prev;
    },
    [] as Array<ThrusterConfiguration>
  );

// ========== Helpers
/**
 * Returns the angular acceleration,
 * multiplied by the mass
 */
export const thrusterCoefficient = (thruster: Thruster) => {
  const leverArmVectorLength = thruster.position.radius;
  const leverArmAngle = thruster.position.radius;
  const forceAngle = thruster.angle;

  // TODO: check if this shouldn't be the other way around
  const applicationAngle = leverArmAngle - forceAngle;

  const strength = thruster.strength;

  return (strength * Math.sin(applicationAngle)) / leverArmVectorLength;
};

export type ThrusterCapcities = Record<AxisDirection, number>;

export const maxThrusterCapacities = (
  config: ThrusterConfiguration
): ThrusterCapcities => {
  const total: ThrusterCapcities = {
    [AxisDirection.Negative]: 0,
    [AxisDirection.Positive]: 0,
  };

  for (const thruster of config.thrusters) {
    const coefficient = thrusterCoefficient(thruster);

    total[coefficient > 0 ? AxisDirection.Positive : AxisDirection.Negative] +=
      coefficient;
  }

  return total;
};

/**
 * Adjusts all the thruster for causing an arbitrary rotation,
 * while maximizing the forward acceleration
 */
export const adjustCapacitiesForRotation = (
  config: ThrusterConfiguration,
  rotation: number
): ThrusterCapcities => {
  const capacities: ThrusterCapcities = maxThrusterCapacities(config);

  const target = toDirectionalAngle(
    clamp(
      rotation,
      capacities[AxisDirection.Negative],
      capacities[AxisDirection.Positive]
    )
  );

  const clampedCapacities = {
    [AxisDirection.Positive]: Math.min(
      capacities[AxisDirection.Positive],
      Math.max(target, 0)
    ),
    [AxisDirection.Negative]: Math.max(
      capacities[AxisDirection.Negative],
      Math.min(target, 0)
    ),
  };

  return clampedCapacities;
};
