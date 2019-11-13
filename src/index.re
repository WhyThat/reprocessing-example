open Reprocessing;

let screenWidth = 610;
let screenHeight = 813;
let fruitSize = 90;

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
  | Bomb
  | Explosion;

type gameState = {
  fruitImages: list((fruitType, imageT)),
  slicedFruitImages: list((fruitType, (imageT, imageT))),
  background: imageT,
  explosionImage: imageT,
  bombImage: imageT,
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
    | Slice(fruitType, RightHalf) =>
      getFruitAssetName(fruitType) ++ "_half_2"
    | Bomb => "bomb"
    | Explosion => "explosion"
    };
  "./src/assets/" ++ basePath ++ ".png";
};

let files = [Banana, Apple, Orange, Pineapple, Watermelon, Coconut];

let loadFruits = env =>
  files
  |> List.map(fruitType =>
       (
         fruitType,
         Draw.loadImage(
           ~filename=getGameObjectAssetPath(Fruit(fruitType)),
           ~isPixel=true,
           env,
         ),
       )
     );

let loadSlicedFruits = env =>
  files
  |> List.map(fruitType =>
       (
         fruitType,
         (
           Draw.loadImage(
             ~filename=getGameObjectAssetPath(Slice(fruitType, LeftHalf)),
             ~isPixel=true,
             env,
           ),
           Draw.loadImage(
             ~filename=getGameObjectAssetPath(Slice(fruitType, RightHalf)),
             ~isPixel=true,
             env,
           ),
         ),
       )
     );


let setup = env => {
  Env.size(~width=screenWidth, ~height=screenHeight, env);
  let fruitImages = loadFruits(env);
  let slicedFruitImages = loadSlicedFruits(env);
  {
    fruitImages,
    slicedFruitImages,
    background:
      Draw.loadImage(
        ~filename="./src/assets/background.png",
        ~isPixel=true,
        env,
      ),
    explosionImage:
      Draw.loadImage(
        ~filename=getGameObjectAssetPath(Explosion),
        ~isPixel=true,
        env,
      ),
    bombImage:
      Draw.loadImage(
        ~filename=getGameObjectAssetPath(Bomb),
        ~isPixel=true,
        env,
      ),
  };
};

let draw = (state, env) => {
  Draw.image(state.background, ~pos=(0, 0), env);
  state;
};

run(~setup, ~draw, ());
