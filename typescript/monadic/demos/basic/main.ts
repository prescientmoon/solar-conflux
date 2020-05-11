import { runUi, makeComponent, Dispatcher, mkChild } from '../../src';
import { render, html, TemplateResult } from 'lit-html';

type TodoState = {
  name: string;
  done: boolean;
};

type TodoAction = 'complete';

const todo = makeComponent(
  ({ name, done }: TodoState, dispatch: Dispatcher<TodoAction>) => {
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

const todoList = makeComponent(
  (state: TodoListState, dispatch: Dispatcher<TodoListAction>, child) => {
    return html`
      <div>
        <input
          value="${state.inputValue}"
          @input=${({ value }) => dispatch({ label: 'setInput', value })}
        />
        <button @click=${() => dispatch('create')}>Add todo</button>
        <div>
          ${state.todos.map((_, index) => child('todo', String(index), index))}
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
);

runUi({
  parent: document.getElementById('app'),
  render,
  initialState: { todos: [], inputValue: '' },
  component: todoList,
});
