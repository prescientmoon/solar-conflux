import { CircularBuffer } from "../../CircularBuffer";
import { settings } from "../common/Settings";
import { shootWand } from "../GameAction";
import { EntityId, SimulationState } from "../State";
import {
  Card,
  CardId,
  CardRef,
  CastState,
  mergeStatsMut,
  noStats,
  ProjectileKind,
  Wand,
  WandId,
  WandState,
} from "../wand";
import * as V from "../common/Vector";

function getCard(state: SimulationState, cardId: CardId): Card {
  const card = state.cards[cardId];

  if (card === undefined) throw new Error(`Cannot find card ${cardId}`);

  return card;
}

function getWand(state: SimulationState, wandId: WandId): Wand {
  const wand = state.wands[wandId];

  if (wand === undefined) throw new Error(`Cannot find wand ${wandId}`);

  return wand;
}

/** Create a cast state with nothing attached to it (yet) */
function emptyCastState(): CastState {
  return {
    accumulatedTransform: {
      direction: 0,
      position: V.origin(),
    },
    stats: noStats(),
    forceRecharge: false,
    projectiles: [],
  };
}

function resetWandState(wand: Wand, wandState: WandState) {
  wandState.deck.clear();
  wandState.deck.pushMany(
    wand.cards.map((id, index) => ({
      index,
      id,
    }))
  );
}

function emptyWandState(state: SimulationState, wandId: WandId): WandState {
  const wand = getWand(state, wandId);

  const wandState: WandState = {
    discarded: new CircularBuffer(settings.maxDeckSize),
    hand: new CircularBuffer(settings.maxDeckSize),
    deck: new CircularBuffer(settings.maxDeckSize),
    mana: wand.maxMana,
  };

  resetWandState(wand, wandState);

  return wandState;
}

function draw(wandState: WandState, castState: CastState): CardRef | null {
  const card = wandState.deck.tryPopFirst();

  // Wrapping:
  //
  // When a modifier/multicast/trigger/timer tries to
  // draw a card but there are no cards in the deck,
  // we add all the cards in the discard pile
  // (in the order they had in the original deck)
  //
  // This allows drawing from the "start" of the deck once the deck is empty,
  // which is why it's called "wrapping"
  //
  // A flag is set on the cast state so we recharge once this current cast is over
  // This ensures the recharge delay can't be straight up avoided
  // by endnig a wand with a draw-card
  //
  // This is the reason the hand is a thing -
  // we don't want to allow infinite recursion
  // were a spell keeps calling itself.
  if (card === null) {
    if (!wandState.discarded.size) return null; // Nothing to wrap!

    const discarded = wandState.discarded.toArray(); // Save discarded pile into array
    wandState.discarded.clear(); // Empty out discarded pile

    discarded.sort((a, b) => a.index - b.index); // Reorder the discard-pile in the order the cards were in in the original deck
    wandState.deck.pushMany(discarded); // Add the discarded pile to the deck in the correct order

    castState.forceRecharge = true; // Force the wand to automatically recharge once this cast is over

    return draw(wandState, castState);
  }

  wandState.hand.push(card);

  return card;
}

function drawAndUpdateCastState(
  state: SimulationState,
  castState: CastState,
  wandState: WandState,
  wand: Wand
) {
  const cardRef = draw(wandState, castState);

  if (cardRef === null) return; // If no more cards to draw, just halt execution

  const cardId = cardRef.id;
  const card = getCard(state, cardId);

  // Pay mana cost
  if (wandState.mana >= card.manaCost) {
    wandState.mana -= card.manaCost;
    for (const effect of card.effects) {
      if (effect.type === "multicast") {
        for (const cast of effect.formation) {
          // Save old transform
          const old = castState.accumulatedTransform;

          // Apply changes to current transform
          castState.accumulatedTransform = {
            position: V.add(
              old.position,
              V.rotate(cast.position, old.direction)
            ),
            direction: old.direction + cast.direction,
          };

          // Go deeper
          drawAndUpdateCastState(state, castState, wandState, wand);

          // Restore transform
          castState.accumulatedTransform = old;
        }
      } else if (effect.type === "projectile") {
        let continuation: ProjectileKind<CastState>;

        if (effect.kind.type === "normal") {
          continuation = effect.kind;
        } else {
          // Trigger and timer projectiles create an isolated cast state
          const innerCastState = emptyCastState();
          drawAndUpdateCastState(state, innerCastState, wandState, wand);

          continuation = {
            ...effect.kind,
            payload: innerCastState,
          };
        }

        castState.projectiles.push({
          position: V.origin(),
          direction: 0,
          blueprint: effect.blueprint,
          continuation: continuation!,
        });
      } else if (effect.type === "modifier") {
        mergeStatsMut(castState.stats, castState.stats, effect.stats);
        drawAndUpdateCastState(state, castState, wandState, wand);
      }
    }
  } else drawAndUpdateCastState(state, castState, wandState, wand); // If not enough mana on the wand, skip to the next spell
}

export function castWand(state: SimulationState, eid: EntityId) {
  console.log("here");
  const castState = emptyCastState();
  const wandState = state.components.wandHolder.wandState[eid];
  const wand = state.wands[state.components.wandHolder.wandId[eid]];

  drawAndUpdateCastState(state, castState, wandState, wand);

  if (wandState.deck.size === 0 || castState.forceRecharge) {
    console.log("Recharge!!!");

    resetWandState(wand, wandState);
  }

  console.log({ castState, wandState });
}

export function spawnWand(state: SimulationState, wandId: WandId) {
  console.log("here");
  const wid = state.ecs.createEntity(); // The id of the wand

  state.ecs.addComponent(wid, state.components.wandHolder);

  const wand = getWand(state, wandId);

  state.components.wandHolder.wandId[wid] = wandId;
  state.components.wandHolder.wandState[wid] = emptyWandState(state, wandId);

  state.tickScheduler.schedule(state.tick + wand.castDelay, shootWand(wid));

  return wid;
}
