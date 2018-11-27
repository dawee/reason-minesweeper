let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let rows = 10;
let cols = 20;
let bombsCount = 20;

type buttonState =
  | Initial
  | Clicked;

type button = {
  case: Case.t,
  state: buttonState
};

let coords =
  range(0, rows)
  |> List.map(
    row => range(0, cols) |> List.map(col => (row, col))
  )
  |> List.flatten;

let notEquals = (constant, value) => value != constant;

let without = toRemove => notEquals(toRemove) -> List.filter;

let rec generateBombsCoords = (~bombsCount=bombsCount, ~coords=coords, ~bombsCoords:list((int, int))=[], ()) => {
  if (bombsCount < 1) {
    bombsCoords
  } else {
    let pickedIndex = coords |> List.length |> Js.Math.random_int(0);
    let picked = List.nth(coords, pickedIndex);

    generateBombsCoords(
      ~bombsCount=bombsCount - 1,
      ~coords=coords |> without(picked),
      ~bombsCoords=bombsCoords |> List.append([picked]),
      ()
    );
  }
};

let bombsCoords = generateBombsCoords();

let hasBombThere = (~bombsCoords=bombsCoords, row, col) =>
  bombsCoords |> List.mem((row, col));

let bombsInCell = (~bombsCoords=bombsCoords, row, col) =>
  hasBombThere(~bombsCoords=bombsCoords, row, col) ? 1 : 0;

let aroundOffsets = [
  (-1, 0),
  (-1, 1),
  (0, 1),
  (1, 1),
  (1, 0),
  (1, -1),
  (0, -1),
  (-1, -1),
];

let countBombsAround = (~bombsCoords=bombsCoords, row, col) =>
  aroundOffsets
  |> List.fold_left((count, offset) => {
    let (rowOffset, colOffset) = offset;
    count + bombsInCell(~bombsCoords=bombsCoords, row + rowOffset, col + colOffset); 
  }, 0);

let computeCase = (~bombsCoords=bombsCoords, row, col) =>
  if (hasBombThere(~bombsCoords=bombsCoords, row, col)) {
    Case.Bomb
  } else {
    let bombsAround = countBombsAround(~bombsCoords=bombsCoords, row, col);

    bombsAround == 0 ? Case.Empty : Case.BombsAround(bombsAround); 
  };

type board = list(list(button));

let board: board =
  range(0, rows)
  |> List.map(
    row =>
      range(0, cols)
      |> List.map(col => {
        {
          state: Initial,
          case: computeCase(row, col)
        }
      })
  );

let rec reveal = (board: board, row: int, col: int) =>
  checkCoordAndReveal(board, row, col)
and checkCoordAndReveal = (board: board, row: int, col: int) =>
  !validCoord(row, col)
  ? board
  : checkButtonStateAndReveal(board, row, col)
and checkButtonStateAndReveal = (board: board, row: int, col: int) => {
  let button = getButton(board, row, col);

  switch (button.state) {
  | Initial => revealButtonCase(board, row, col, button)
  | Clicked => board
  };
}
and revealButtonCase = (board, row, col, button) => {
  let board = setButtonState(board, row, col, Clicked);

  switch (button.case) {
  | Empty =>
    aroundOffsets
    |> List.fold_left(
      (board, offset) => {
        let (rowOffset, colOffset) = offset;
        
        reveal(board, row + rowOffset, col + colOffset);
      },
      board
    )
  | _ => board
  };
}
and setButtonState = (board, row, col, state) =>
  board
  |> List.mapi(
    (row_, cells) => {
      cells
      |> List.mapi(
        (col_, button) => {
          row_ == row && col_ == col
          ? {...button, state }
          : button
        }
      )
    }
  )
and getButton = (board, row, col) =>
  board
  -> List.nth(row)
  -> List.nth(col)
and validCoord = (row: int, col: int) =>
  coords |> List.mem((row, col))

type state = { board };
type action =
  | Click(int, int);

let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,
  initialState: () => { board: board },

  reducer: (action: action, state: state) => {
    switch(action) {
    | Click(row: int, col: int) => ReasonReact.Update({
        board: reveal(state.board, row, col)
      }) 
    }

  },

  render: self =>
    <div className="board">
      (
        self.state.board
        |> List.mapi(
          (row, buttons) => {
            buttons
            |> List.mapi(
              (col, button) => {
                let cell =
                  switch (button.state) {
                  | Initial => <button className="initial" onClick={_event => self.send(Click(row, col))} />
                  | Clicked => <Case case=button.case />
                  };

                {
                  <div className="cell">
                  (cell)
                  </div>
                }
              }
            )
            |> Array.of_list
            |> ReasonReact.array
            |> buttons => (
              <div className="row">(buttons)</div>
            )
          }
        )
        |> Array.of_list
        |> ReasonReact.array
      )
    </div>,
};