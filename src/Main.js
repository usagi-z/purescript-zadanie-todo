// data Node :: Type

// persistTodos :: Array TodoItem -> Effect Unit
exports.persistTodos = function (todos) {
    localStorage.setItem('todos',JSON.stringify(todos));
};

// loadTodos :: Effect (Array TodoItem)
exports.loadTodos = function () {
    return JSON.parse(localStorage.getItem('todos'));
}

// window :: Effect Node
exports.window = function () {
  return window;
};
