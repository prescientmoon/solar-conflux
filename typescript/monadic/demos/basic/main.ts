import { runUi } from '../../src';
import { render, html, TemplateResult } from 'lit-html';

type Action = 'increase' | 'decrease';

type State = {
  count: number;
};

runUi<TemplateResult, State, Action>({
  parent: document.getElementById('app'),
  render,
  initialState: { count: 0 },
  component: {
    render: (state, dispatch) => {
      return html`
        <div>
          <div>${state.count}</div>
          <button @click=${dispatch('increase')}>Increase</button>
          <button @click=${dispatch('decrease')}>Decrease</button>
        </div>
      `;
    },
    handleActions: (action: Action, state) => {
      if (action === 'increase') {
        return {
          count: state.count + 1,
        };
      } else if (action === 'decrease') {
        return {
          count: state.count - 1,
        };
      }

      return state;
    },
  },
});
