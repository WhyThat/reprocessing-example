open Reprocessing;

let screenWidth = 610;
let screenHeight = 813;
let fruitSize = 80;
let gravity = 0.3;
let fruitTimer = 2.;

type fruit = {
  pos: (float, float),
  velocity: (float, float),
  image: imageT,
  slicedImage: (imageT, imageT),
};

type gameState = {
  fruitImages: array(imageT),
  slicedFruitImages: array((imageT, imageT)),
  fruits: list(fruit),
  slicedFruits: list(fruit),
  background: imageT,
  mousePressed: bool,
  lastWave: float,
  time: float,
};

let files = [|"apple", "banana", "coconut", "orange", "pineapple", "watermelon"|];

let loadFruits = env =>
  files |> Array.map(file => Draw.loadImage(~filename="./src/assets/" ++ file ++ ".png", ~isPixel=true, env));

let loadSlicedFruits = env =>
  files
  |> Array.map(file =>
       (
         Draw.loadImage(~filename="./src/assets/" ++ file ++ "_half_1.png", ~isPixel=true, env),
         Draw.loadImage(~filename="./src/assets/" ++ file ++ "_half_2.png", ~isPixel=true, env),
       )
     );

let generateFruit = (fruitImages, slicedFruitImages) => {
  let posX = Utils.randomf(~min=0., ~max=float_of_int(screenWidth));
  let isInLeftHalf = posX <= float_of_int(screenWidth) /. 2.;
  let fruitIndex = Utils.random(~min=0, ~max=Array.length(fruitImages));
  {
    image: fruitImages[fruitIndex],
    slicedImage: slicedFruitImages[fruitIndex],
    pos: (posX, float_of_int(screenHeight)),
    velocity: (
      Utils.randomf(~min=isInLeftHalf ? 0. : (-7.), ~max=isInLeftHalf ? 7. : 0.),
      Utils.randomf(~min=-20., ~max=-15.),
    ),
  };
};

let generateSlicedFruit = fruit => {
  let (firstHalf, secondHalf) = fruit.slicedImage;
  let (velX, velY) = fruit.velocity;
  [
    {...fruit, image: firstHalf, velocity: (velX -. 2., velY +. 1.)},
    {...fruit, image: secondHalf, velocity: (velX +. 2., velY -. 1.)},
  ];
};

let setup = env => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  let fruitImages = loadFruits(env);
  let slicedFruitImages = loadSlicedFruits(env);
  {
    fruitImages,
    slicedFruitImages,
    background: Draw.loadImage(~filename="./src/assets/background.png", ~isPixel=true, env),
    fruits: [],
    slicedFruits: [],
    mousePressed: false,
    lastWave: 0.,
    time: 0.,
  };
};

let renderFruits = (fruits, env) => {
  fruits
  |> List.iter(fruit => {
       let (x, y) = fruit.pos;
       Draw.image(fruit.image, ~pos=(int_of_float(x), int_of_float(y)), ~width=fruitSize, ~height=fruitSize, env);
     });
};

let isMouseIn = ((x, y), env) => {
  let (mouseX, mouseY) =
    switch (Env.pmouse(env)) {
    | (x, y) => (float_of_int(x), float_of_int(y))
    };
  mouseX >= x && mouseX <= x +. float_of_int(fruitSize) && mouseY >= y && mouseY <= y +. float_of_int(fruitSize);
};

let updateFruits = (fruits, env) =>
  fruits
  |> List.map(fruit => {
       let (x, y) = fruit.pos;
       let (velX, velY) = fruit.velocity;
       let newVelY = velY +. gravity;
       {...fruit, pos: (x +. velX, y +. velY), velocity: (velX, newVelY)};
     })
  |> List.filter(fruit => {
       let (_, y) = fruit.pos;
       y <= float_of_int(screenHeight);
     });

let rec generateNFruits = (size, fruitList, fruitImages, slicedFruitImages) =>
  size > 0
    ? [
      generateFruit(fruitImages, slicedFruitImages),
      ...generateNFruits(size - 1, fruitList, fruitImages, slicedFruitImages),
    ]
    : fruitList;

let draw = (state, env) => {
  Draw.image(state.background, ~pos=(0, 0), env);
  renderFruits(state.fruits, env);
  renderFruits(state.slicedFruits, env);

  let time = state.time +. Env.deltaTime(env);
  let launchWave = state.time > state.lastWave +. fruitTimer;

  let slicedFruits =
    state.fruits
    |> List.filter(fruit => isMouseIn(fruit.pos, env))
    |> List.fold_left((acc, fruit) => List.concat([acc, generateSlicedFruit(fruit)]), state.slicedFruits);
  Draw.text(~body=string_of_int(List.length(slicedFruits)), ~pos=(0, 0), env);

  let newFruits =
    launchWave
      ? generateNFruits(Utils.random(~min=1, ~max=5), state.fruits, state.fruitImages, state.slicedFruitImages)
      : state.fruits;

  let newState = {
    ...state,
    time,
    lastWave: launchWave ? time : state.lastWave,
    mousePressed: Env.mousePressed(env),
    slicedFruits: updateFruits(slicedFruits, env),
    fruits: updateFruits(newFruits |> List.filter(fruit => !isMouseIn(fruit.pos, env)), env),
  };

  newState;
};

run(~setup, ~draw, ());
