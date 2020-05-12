import {
  runUi,
  makeComponent,
  Dispatcher,
  ComponentSpec,
  withAction,
  mkChild,
  buildSpec,
  withState,
  emptySpec,
  withTemplate,
  withChild,
} from '../../src';
import { render, html, TemplateResult } from 'lit-html';
import { pipe } from 'fp-ts/lib/pipeable';

const baseSpec = pipe(emptySpec, withTemplate<TemplateResult>());

type TodoState = {
  name: string;
  done: boolean;
};

type TodoAction = 'complete';

const todo = pipe(
  baseSpec,
  withAction<TodoAction>(),
  withState<TodoState>(),
  buildSpec(
    ({ name, done }, { dispatch }) => {
      return html`
        <div>
          Name: ${name} Completed: ${done}
          <button @click=${() => dispatch('complete')}>Complete todo</button>
        </div>
      `;
    },
    (action, state) => {
      if (action === 'complete') {
        return {
          ...state,
          done: true,
        };
      }

      return state;
    },
    {}
  )
);

type TodoListState = {
  todos: TodoState[];
  inputValue: string;
};

type TodoListAction =
  | {
      label: 'setInput';
      value: string;
    }
  | 'create';

const todoList = pipe(
  emptySpec,
  withAction<TodoListAction>(),
  withState<TodoListState>(),
  withChild<'todo', number, TodoState, null>(),
  buildSpec(
    (state, { dispatch, child }) => {
      return html`
        <div>
          <input
            value="${state.inputValue}"
            @input=${({ value }) => dispatch({ label: 'setInput', value })}
          />
          <button @click=${() => dispatch('create')}>Add todo</button>
          <div>
            ${state.todos.map((_, index) =>
              child('todo', String(index), index)
            )}
          </div>
        </div>
      `;
    },
    (action: TodoListAction, state: TodoListState) => {
      if (action === 'create') {
        return {
          inputValue: '',
          todos: [
            ...state.todos,
            {
              name: state.inputValue,
              done: false,
            },
          ],
        };
      } else if (action.label === 'setInput') {
        return {
          ...state,
          inputValue: action.value,
        };
      }

      return state;
    },
    {
      todo: mkChild(
        (index: number) => ({
          get: ({ todos }) => todos[index],
          set: (state, newTodoState) => ({
            ...state,
            todos: state.todos.map((todoState, currentIndex) =>
              index === currentIndex ? newTodoState : todoState
            ),
          }),
        }),
        todo
      ),
    }
  )
);

runUi({
  parent: document.getElementById('app'),
  render,
  initialState: { todos: [], inputValue: '' },
  component: todoList,
});
