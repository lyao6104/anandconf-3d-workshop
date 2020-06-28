-- Skybox2.elm from the "3d-elm-camp" repo

module Skybox exposing (skybox)

{-| This example shows how to load mulitiple textures 
    and apply it to a simple 3D entity (Box).
-}

import Angle exposing (Angle)
import Array exposing (Array)
import Browser
import Camera3d
import Color exposing (Color)
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Task
import Viewpoint3d
import WebGL.Texture

type alias Model = 
    { 
     textures : Maybe (List (Material.Texture Color))
    }


type Msg
    = LoadTexture (List (Material.Texture Color))
      | Error WebGL.Texture.Error


textureBottom =
    "texture/sky-box-Bottom.jpg"
textureTop =
    "texture/sky-box-Top.jpg"
textureSide1 =
    "texture/sky-box-Side1.jpg"
textureSide2 =
    "texture/sky-box-Side2.jpg"
textureSide3 =
    "texture/sky-box-Side3.jpg"
textureSide4 =
    "texture/sky-box-Side4.jpg"
textureListSkyBox = 
  [textureBottom, textureTop, textureSide1, textureSide2, textureSide3
    , textureSide4]


init : () -> ( Model, Cmd Msg )
init () =
    ( {textures = Nothing}
    , fetchTextures
    )

-- Fetch the texture from the List, textureListSkyBox.
-- Get a result type as List (Material.Texture Color)
-- Decode the List when we actually are going to load the texture
-- In this example, we decode the list in Skybox2.skybox
fetchTextures : Cmd Msg 
fetchTextures =
  textureListSkyBox
    |> List.map Material.load
    -- Load the meterial, [Material.load texture, Material.load texture... ]
    |> Task.sequence -- sequence : List (Task x a) -> Task x (List a)
    -- Transform a list of the tast to a tast
    -- Get the result type as Task WebGL.Texture.Error (List (Texture value))
    |> Task.andThen -- andThen :
    -- concatenate two tasks
         (\textures -> 
            case textures of
              [] -> 
                Task.fail WebGL.Texture.LoadError
              textList ->
                Task.succeed textList)
              -- If the list is not empty let the tast succeed
    |> Task.attempt -- Attempt to update the task here
       (\result ->
            case result of
                Ok texture -> LoadTexture texture
                Err error -> Error error
        )

       
    
update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
       Error err -> 
         ( model, Cmd.none)
       LoadTexture textures -> 
         ( { model | textures = Just textures }, Cmd.none)


view : Model -> Browser.Document Msg
view model =
    let
        -- Construct a fixed camera
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0.5 0.5 0
                        , azimuth = Angle.degrees 20
                        , elevation = Angle.degrees 60
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "Texture"
    , body =
        case model.textures of
            Just texturesList  ->
                -- Texture loaded successfully, render a scene using it
                    [ Scene3d.unlit
                      { camera = camera
                       , dimensions = ( Pixels.pixels 400, Pixels.pixels 400 )
                       , background = Scene3d.transparentBackground
                       , clipDepth = Length.meters 0.1
                       , entities =
                        [ -- Use the loaded texture as the material for the skybox
                          skybox texturesList 100
                        ]
                       }
                    ]
            Nothing ->
                [ Html.text "Loading..." ]
    }

skybox textureList size =
    let
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.centimeters -size

        positive =
            Length.centimeters size

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces with different colors
        skybottom texture =
            Scene3d.quad texture p4 p3 p2 p1

        skytop texture =
            Scene3d.quad texture p8 p7 p6 p5
        -- skyside1 to skyside4, see sky-box-order.jpg as the
        -- order reference
        -- corresponding to the picture from left to right
        skyside1 texture =
            Scene3d.quad texture p1 p4 p8 p5

        skyside2 texture =
            Scene3d.quad texture p4 p3 p7 p8

        skyside3 texture =
            Scene3d.quad texture p3 p2 p6 p7

        skyside4 texture =
            Scene3d.quad texture p2 p1 p5 p6
    in
      -- Combine all faces into a single entity
      -- Decode the list here
      case textureList of
         bottom :: top :: side1 :: side2 :: side3 :: side4 :: _ ->
            let
              debottom = (Material.texturedColor (bottom))
              detop = (Material.texturedColor (top))
              deside1 = (Material.texturedColor (side1))
              deside2 = (Material.texturedColor (side2))
              deside3 = (Material.texturedColor (side3))
              deside4 = (Material.texturedColor (side4))
            in
              Scene3d.group [ skybottom debottom, skytop detop
                , skyside1 deside1, skyside2 deside2
                , skyside3 deside3, skyside4 deside4 ]
         _ ->
            let
              bottom = Material.matte Color.white
              top = Material.matte Color.white
              side1 = Material.matte Color.white
              side2 = Material.matte Color.white
              side3 = Material.matte Color.white
              side4 = Material.matte Color.white
            in
              Scene3d.group [ skybottom bottom, skytop top
                , skyside1 side1, skyside2 side2
                , skyside3 side3, skyside4 side4 ]

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
