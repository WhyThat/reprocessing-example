open Reprocessing;

let screenWidth = 610;
let screenHeight = 813;
let fruitSize = 90;
let gravity = 0.2;
let fruitTimer = 2.;

type fruitType =
  | Banana
  | Apple
  | Orange
  | Pineapple
  | Watermelon
  | Coconut;

type sliceType =
  | LeftHalf
  | RightHalf;

type kind =
  | Fruit(fruitType)
  | Slice(fruitType, sliceType)
  | Bomb;

type vector2 = (float, float);

type gameObject = {
  pos: vector2,
  velocity: vector2,
  angularVelocity: float,
  angle: float,
  image: imageT,
  kind,
};

type gameState = {
  fruitImages: list((fruitType, imageT)),
  slicedFruitImages: list((fruitType, (imageT, imageT))),
  gameObjects: list(gameObject),
  background: imageT,
  bombImage: imageT,
  mousePressed: bool,
  lastWave: float,
  time: float,
  blade: list(vector2),
};

let getFruitAssetName = fruit =>
  switch (fruit) {
  | Banana => "banana"
  | Apple => "apple"
  | Orange => "orange"
  | Pineapple => "pineapple"
  | Watermelon => "watermelon"
  | Coconut => "coconut"
  };

let getGameObjectAssetPath = go => {
  let basePath =
    switch (go) {
    | Fruit(fruitType) => getFruitAssetName(fruitType)
    | Slice(fruitType, LeftHalf) => getFruitAssetName(fruitType) ++ "_half_1"
    | Slice(fruitType, RightHalf) => getFruitAssetName(fruitType) ++ "_half_2"
    | Bomb => "bomb"
    };
  "./src/assets/" ++ basePath ++ ".png";
};

let files = [Banana, Apple, Orange, Pineapple, Watermelon, Coconut];

let loadFruits = env =>
  files
  |> List.map(fruitType =>
       (fruitType, Draw.loadImage(~filename=getGameObjectAssetPath(Fruit(fruitType)), ~isPixel=true, env))
     );

let loadSlicedFruits = env =>
  files
  |> List.map(fruitType =>
       (
         fruitType,
         (
           Draw.loadImage(~filename=getGameObjectAssetPath(Slice(fruitType, LeftHalf)), ~isPixel=true, env),
           Draw.loadImage(~filename=getGameObjectAssetPath(Slice(fruitType, RightHalf)), ~isPixel=true, env),
         ),
       )
     );

let getImageInList = (imageList, key) =>
  imageList
  |> List.find(((t, _)) => t == key)
  |> (
    fun
    | (_, image) => image
  );

let getRandomFruitType = () =>
  switch (Utils.random(~min=0, ~max=List.length(files) - 1)) {
  | 0 => Banana
  | 1 => Apple
  | 2 => Orange
  | 3 => Pineapple
  | 4 => Watermelon
  | 5 => Coconut
  | _ => Banana
  };

let generateFruit = (fruitImages, bombImage) => {
  let posX = Utils.randomf(~min=0., ~max=float_of_int(screenWidth));
  let isInLeftHalf = posX <= float_of_int(screenWidth) /. 2.;
  let fruitType = getRandomFruitType();
  let kind =
    switch (Utils.random(~min=0, ~max=10)) {
    | 0 => Bomb
    | _ => Fruit(fruitType)
    };
  {
    image:
      switch (kind) {
      | Fruit(fruitType) => getImageInList(fruitImages, fruitType)
      | _ => bombImage
      },
    pos: (posX, float_of_int(screenHeight)),
    angularVelocity: Utils.randomf(~min=0., ~max=0.3),
    angle: 0.,
    velocity: (
      Utils.randomf(~min=isInLeftHalf ? 0. : (-5.), ~max=isInLeftHalf ? 5. : 0.),
      Utils.randomf(~min=-16., ~max=-13.),
    ),
    kind,
  };
};

let rec generateNFruits = (size, fruitList, fruitImages, bombImage) =>
  size > 0
    ? [generateFruit(fruitImages, bombImage), ...generateNFruits(size - 1, fruitList, fruitImages, bombImage)]
    : fruitList;

let generateSlicedFruit = (fruit, slicedFruitImages) => {
  let (firstHalf, secondHalf) =
    switch (fruit.kind) {
    | Fruit(fruitType) => getImageInList(slicedFruitImages, fruitType)
    | _ => getImageInList(slicedFruitImages, Banana)
    };
  let (velX, velY) = fruit.velocity;
  let fruitType =
    switch (fruit.kind) {
    | Fruit(fruitType) => fruitType
    | _ => Banana
    };
  [
    {...fruit, image: firstHalf, velocity: (velX -. 2., velY +. 1.), kind: Slice(fruitType, LeftHalf)},
    {...fruit, image: secondHalf, velocity: (velX +. 2., velY -. 1.), kind: Slice(fruitType, RightHalf)},
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
    bombImage: Draw.loadImage(~filename="./src/assets/bomb.png", ~isPixel=true, env),
    gameObjects: [],
    mousePressed: false,
    lastWave: 0.,
    time: 0.,
    blade: [],
  };
};

let renderGameObject = (gmaeObjects, env) => {
  gmaeObjects
  |> List.iter(gameObject => {
       let (x, y) = gameObject.pos;
       Draw.pushMatrix(env);
       Draw.translate(~x, ~y, env);
       Draw.rotate(gameObject.angle, env);
       Draw.image(
         gameObject.image,
         ~pos=(- fruitSize / 2, - fruitSize / 2),
         ~width=fruitSize,
         ~height=fruitSize,
         env,
       );
       Draw.popMatrix(env);
     });
};

let getMouseF = env =>
  switch (Env.pmouse(env)) {
  | (x, y) => (float_of_int(x), float_of_int(y))
  };

let isMouseIn = ((x, y), env) => {
  let (mouseX, mouseY) = getMouseF(env);
  let fruitSizeF = float_of_int(fruitSize);
  mouseX >= x
  -. fruitSizeF
  /. 2.
  && mouseX <= x
  +. fruitSizeF
  /. 2.
  && mouseY >= y
  -. fruitSizeF
  /. 2.
  && mouseY <= y
  +. fruitSizeF
  /. 2.;
};

let updateFruits = (slicedFruitImages, env, gameObjects) =>
  gameObjects
  |> List.fold_left(
       (acc, gameObject) =>
         switch (gameObject.kind) {
         | Fruit(_) =>
           Env.mousePressed(env) && isMouseIn(gameObject.pos, env)
             ? acc @ generateSlicedFruit(gameObject, slicedFruitImages) : acc @ [gameObject]
         | _ => acc @ [gameObject]
         },
       [],
     );

let updateGameObjects = gameObjects =>
  gameObjects
  |> List.map(gameObject => {
       let (x, y) = gameObject.pos;
       let (velX, velY) = gameObject.velocity;
       let newVelY = velY +. gravity;
       {
         ...gameObject,
         pos: (x +. velX, y +. velY),
         velocity: (velX, newVelY),
         angle: gameObject.angle +. gameObject.angularVelocity,
       };
     })
  |> List.filter(gameObject => {
       let (_, y) = gameObject.pos;
       y <= float_of_int(screenHeight);
     });

let updateFruitWaves = (~launchWave, ~fruitImages, ~bombImage, gameObjects) =>
  launchWave ? generateNFruits(Utils.random(~min=1, ~max=5), gameObjects, fruitImages, bombImage) : gameObjects;

let draw = (state, env) => {
  Draw.image(state.background, ~pos=(0, 0), env);
  state.blade
  |> List.iteri((i, point) => {
       Draw.stroke({r: 100., g: 0., b: 0., a: 1.}, env);
       Draw.strokeWeight(i, env);
       Draw.linef(
         ~p1=i >= List.length(state.blade) - 1 ? getMouseF(env) : List.nth(state.blade, i + 1),
         ~p2=point,
         env,
       );
     });
  renderGameObject(state.gameObjects, env);

  let time = state.time +. Env.deltaTime(env);
  let launchWave = state.time > state.lastWave +. fruitTimer;

  let newGameObjects =
    state.gameObjects
    |> updateFruits(state.slicedFruitImages, env)
    |> updateFruitWaves(~launchWave, ~fruitImages=state.fruitImages, ~bombImage=state.bombImage)
    |> updateGameObjects;

  Draw.text(~body=string_of_int(List.length(state.blade)), ~pos=(0, 0), env);

  let newBlade = Env.mousePressed(env) ? List.append(state.blade, [getMouseF(env)]) : state.blade;

  let newState = {
    ...state,
    time,
    lastWave: launchWave ? time : state.lastWave,
    mousePressed: Env.mousePressed(env),
    gameObjects: newGameObjects,
    blade:
      List.length(newBlade) > 8 || !Env.mousePressed(env) && List.length(newBlade) > 0
        ? List.tl(newBlade) : newBlade,
  };

  newState;
};

run(~setup, ~draw, ());
