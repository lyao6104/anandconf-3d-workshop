{- COMPILED BUT NOT SHOWN -}

{- This file should provide a good starting point for the snowman. Modified from "3d-elm-camp/BeeMovement.elm".
 -}

module WorkshopTemplate exposing (main)

-- Most of these imports were taken from "3d-elm-camp/BeeMovement.elm", so there may be a lot of unused things
import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import Sphere3d
import Task
import Temperature
import Vector3d exposing (Vector3d)
import Viewpoint3d
import Cone3d
import Point2d
import Arc2d
import Arc3d
import Circle3d
import TriangularMesh
import Cylinder3d
import Triangle3d
import LineSegment3d
import WebGL.Texture
import Skybox

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render full screen
          Browser.Events.onResize
            (\width height ->
                Resize
                    (Pixels.pixels width)
                    (Pixels.pixels height)
            )

        -- Subscribe to animation frames to animate the cubes
        , Browser.Events.onAnimationFrameDelta (Duration.seconds >> Tick)

        -- Listen for visibility change events so we can stop orbiting if the
        -- user switches to a different tab etc.
        , Browser.Events.onVisibilityChange VisibilityChange

        -- Listen for orbit-related mouse events
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , time = 0
      , orbiting = False
      , azimuth = Angle.degrees -90
      , elevation = Angle.degrees 30
      , textures = Nothing 
      }
    , Cmd.batch 
        [ Task.perform
            -- The scene gets resized to match the browser window
            (\{ viewport } ->
                Resize
                    (Pixels.int (round viewport.width))
                    (Pixels.int (round viewport.height))
            )
            Browser.Dom.getViewport
        , fetchTextures
        ] 
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        Tick t ->
           let
                tickRate = 
                    Duration.milliseconds 1 |> Quantity.per Duration.second
                
                updatedTime = 
                    Duration.seconds model.time |> Quantity.plus (tickRate |> Quantity.for t)

                timeAsNum = Duration.inSeconds updatedTime

            in
                ( { model | time = timeAsNum }, Cmd.none )
        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        VisibilityChange Browser.Events.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -90) (Angle.degrees 90)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        LoadTexture textures -> 
            ( { model | textures = Just textures }, Cmd.none)

        Error _ -> 
            ( model, Cmd.none)

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.pixels Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.pixels Decode.float))

-- Fetch textures from textureListSkybox
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

{- UNEDITABLE -}

--custom type for material 
type Mat = Metal | NonMetal | Matte 

--custom type for axis
type Axis = X | Y | Z 

--clean up type for (x,y,z)
type alias Dimension = (Float,Float,Float)

type WorldCoordinates
    = WorldCoordinates

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , time : Float
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , textures : Maybe (List (Material.Texture Color))
    }

type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | LoadTexture (List (Material.Texture Color))
    | Error WebGL.Texture.Error

--Drawing basic shapes 
--non-metal material
myMat : Mat -> Color.Color -> Material.Uniform WorldCoordinates
myMat m colour = 
    case m of 
        Metal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
        NonMetal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
        Matte -> Material.matte colour 

--textured non-metal material (some shapes require textured material? )
myTexturedMat : Mat -> Color.Color -> Material.Textured WorldCoordinates
myTexturedMat m colour = 
    case m of 
        Metal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
        NonMetal -> Material.nonmetal { baseColor = colour, roughness = 0.2 }
        Matte -> Material.matte colour 

cube : (Mat, Color.Color) -> Float -> Scene3d.Entity WorldCoordinates
cube (m, colour) size = Scene3d.blockWithShadow (myMat m colour) <|
        Block3d.from
            (Point3d.centimeters 0 0 0)
            (Point3d.centimeters size size size)

prism : (Mat, Color.Color) -> Dimension -> Scene3d.Entity WorldCoordinates 
prism (m, colour) (x,y,z) = 
        Scene3d.blockWithShadow (myMat m colour) <|
                Block3d.with
                    { x1 = Length.centimeters 0
                    , x2 = Length.centimeters x
                    , y1 = Length.centimeters 0
                    , y2 = Length.centimeters y
                    , z1 = Length.centimeters 0
                    , z2 = Length.centimeters z
                    } 

sphere : (Mat, Color.Color) -> Float -> Scene3d.Entity WorldCoordinates
sphere (m, colour) r = 
        (Scene3d.sphereWithShadow (myTexturedMat m colour) <|
            Sphere3d.withRadius (Length.centimeters r) Point3d.origin)
            |> move (0,0,r)

cone : (Mat, Color.Color) -> Axis -> Dimension -> Scene3d.Entity WorldCoordinates
cone (m, colour) axis (b,t,r) = 
        let 
            along = 
                case axis of 
                    X -> Axis3d.x 
                    Y -> Axis3d.y 
                    Z -> Axis3d.z     

        in 
            Scene3d.coneWithShadow (myMat m colour) <|
                Cone3d.along along
                    { base = Length.centimeters b
                    , tip = Length.centimeters t
                    , radius = Length.centimeters r
                    }

cylinder : (Mat, Color.Color) -> Axis -> Dimension -> Scene3d.Entity WorldCoordinates
cylinder (m, colour) axis (s,e,r) =
        let 
            along = 
                case axis of 
                    X -> Axis3d.x 
                    Y -> Axis3d.y 
                    _ -> Axis3d.z    

        in 
            Scene3d.cylinderWithShadow (myMat m colour) <|
                Cylinder3d.along along
                    { start = Length.centimeters s
                    , end = Length.centimeters e 
                    , radius = Length.centimeters r
                    }

{-| Create both a Light and an Entity (a bright glowing sphere) representing a
particular point light
-}
pointLight :
    { position : Point3d Meters WorldCoordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    }
    -> ( Light WorldCoordinates Bool, Entity WorldCoordinates )
pointLight properties =
    let
        -- Create a sphere to represent a light bulb
        lightsphere =
            Sphere3d.atPoint properties.position (Length.millimeters 100)

        -- Calculate the luminance of the sphere surface by dividing the given
        -- total luminous flux of the light by the surface area of the sphere
        -- and by the solid angle of a hemisphere (assuming that each point on
        -- the surface of the bulb emits light equally in all directions)...I
        -- am not 100% sure this is exactly correct =)
        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea lightsphere)

        -- Create an emissive (glowing) material for the sphere
        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance
    in
    ( Light.point (Light.castsShadows True) properties
    , Scene3d.sphere sphereMaterial lightsphere
    )

--Translation 
move : Dimension -> Entity coordinates -> Entity coordinates
move (x,y,z) entity = entity |> Scene3d.translateBy (Vector3d.centimeters x y z)      

rotate : Float -> Float -> Float -> Entity coordinates -> Entity coordinates 
rotate pitch yaw roll entity = 
    entity 
        |> Scene3d.rotateAround Axis3d.x (Angle.radians pitch)  
        |> Scene3d.rotateAround Axis3d.y (Angle.radians roll)  
        |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)

--TODO: Track eneity's position. 
scale : Float -> Entity coordinates -> Entity coordinates 
scale factor entity = entity |> Scene3d.scaleAbout (Point3d.centimeters 0 0 0) factor

-- repeat an animation for a given duration
repeatDuration : Float -> Int -> Float -> Float -> Float
repeatDuration speed duration startPosition time =
  speed * (time - toFloat duration * toFloat (floor time // duration)) + startPosition

textureListSkyBox : List String
textureListSkyBox = 
  [textureBottom, textureTop, textureSide1, textureSide2, textureSide3
    , textureSide4]

view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.centimeters 0 0 100
                , chromaticity = Light.sunlight
                , intensity = LuminousFlux.lumens 10000
                }
    

        -- Rough approximation of sunlight
        thirdLight =
            Light.directional (Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Light.sunlight
                , intensity = Illuminance.lux 100 
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }

        -- Create a quad to act as a 'floor'
        plane =
            Scene3d.quad (Material.matte Color.white)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)            
        
        -- Define camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 20
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        -- If the proper textures aren't loaded for whatever reason, the sky will just be light blue
        texturesList = case model.textures of
            Just textures ->
                textures

            Nothing ->
                List.repeat 6 (Material.constant Color.lightBlue)

        baseEntities = 
            [ firstLightBall
            , plane
            , Skybox.skybox texturesList 1000
            ]

    in
    Scene3d.custom
        { lights = Scene3d.threeLights firstLight thirdLight softLighting
        , camera = camera
        , clipDepth = Length.centimeters 10
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( model.width, model.height )
        , background = Scene3d.backgroundColor Color.lightBlue
        , entities = baseEntities ++ myEntities model
        }

{- EDITABLE -}

-- Consider this the equivalent of "myShapes" on the other slots. You start out with some snowflakes
myEntities model =  
    [ snowflakes
        |> move (10*sin model.time, 0, -(repeatDuration 15 15 0 model.time))
    ]

{- Here you can specify what images to use to create the skybox -}

textureBottom : String
textureBottom =
    "todo"

textureTop : String
textureTop =
    "todo"

textureSide1 : String
textureSide1 =
    "todo"

textureSide2 : String
textureSide2 =
    "todo"

textureSide3 : String
textureSide3 =
    "todo"

textureSide4 : String
textureSide4 =
    "todo"

-- Snowflakes are just small spheres; you can change these if you want
snowflake : Entity WorldCoordinates
snowflake = 
    sphere (Matte, Color.white) 0.5

snowflakes : Entity WorldCoordinates
snowflakes = 
    let
        sfColumn = List.map2
            ( \ x z -> snowflake |> move (x*40,0,100*sin (z*2) + 200))
            (List.map toFloat <| List.range (-5) 5)
            (List.map toFloat <| List.range (-5) 5)
        sfList = List.map2
            ( \ y z -> Scene3d.group sfColumn |> move (0,y*40,100*sin (z*2)))
            (List.map toFloat <| List.range (-5) 5)
            (List.map toFloat <| List.range (-5) 5)
    in
        Scene3d.group sfList