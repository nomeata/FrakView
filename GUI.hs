import Graphics.UI.Gtk hiding (fill, Point, rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix (transformPoint, Matrix(..))
import qualified  Graphics.Rendering.Cairo.Matrix as CM
import Graphics.UI.Gtk.Builder

import Data.Ix
import Data.IORef
import Data.Maybe
import Data.List
import Text.Printf
import System.Random
import Control.Monad
import Control.Applicative ((<$>))
import qualified Data.Text as T

import FrakData
import MatrixRead

import CoroutineT


data DrawingMethod =   DrawSet BaseSet Integer
		     | DrawCairo BaseSet [Int] Int
		     | DrawChaos Int Bool
		     deriving (Eq)
type ScreenConfig = (Maybe DrawingMethod, Bool, Maybe [Int]) 

ifsColors = cycle
	[ (1,0,0)
	, (0,1,0)
	, (0,0,1)
	, (1,1,0)
	]

knownIFS =
	[ ("Sierpinsky", sierpinsky)
	, ("Koch-Kurve", koch)
	, ("Conway", conway)
	]

data BaseSet = Square | Full | Line | Triangle deriving (Show, Eq, Bounded, Enum)

asSet Square   = square
asSet Full     = fullSquare
asSet Line     = lineSet
asSet Triangle = triangle

asCairo Square = rectangle (1/4) (1/4) (1/2) (1/2) >> fill
asCairo Full   = rectangle 0 0 1 1 >> fill
asCairo Line   = rectangle 0 (1/2-1/10) 1 (2/10) >> fill
asCairo triangle = moveTo (1/2) 0 >> lineTo 1 1 >> lineTo 0 1 >> closePath >> fill

drawIFS ifs = do
	setLineWidth (1/1000)
	setSourceRGB 0.5 0.5 0.5
	drawShape
	savedMatrix <- getMatrix
	forM_ (zip ifs ifsColors) $ \(m, (r,g,b)) -> do
		setSourceRGB r g b
		setMatrix savedMatrix
		transform m
		drawShape
  where drawShape = do
	-- Draw some nice orientation something
	rectangle (1/4) (1/4) (1/2) (1/2)
	stroke
	moveTo (1/4) (1/4)
	lineTo (3/4) (3/4)
	stroke
	moveTo (3/4) (1/4)
	lineTo (1/4) (3/4)
	stroke
	moveTo (1/2) (1/2)
	lineTo (1/2) (0)
	stroke
	moveTo (1/2) (0)
	relLineTo (1/18) (1/12)
	relLineTo (-2/18) 0
	closePath
	fill

drawTrace ifs doRender trace = doRender $ do
	forM_ (zip (map (ifs !!) trace) ifsColors) $ \(m,(r,g,b)) -> do
		setSourceRGBA r g b 1
		transform m
		asCairo Full

drawIt ifs doRender (DrawCairo baseSet cylinder n) = do
	unless (null cylinder) $ do
		pausingForM_ 100 (sequence . replicate n $ ifs) $ \ms -> do
			doRender $ do
				setSourceRGB (1/2) (1/2) (1/2)
				mapM transform ms
				asCairo baseSet
	let rest = n - length cylinder
	let cylinderMs = map (ifs !!) cylinder
	pausingForM_ 100 (sequence . replicate rest $ ifs) $ \ms -> do
		doRender $ do
			setSourceRGB 0 0 0 
			mapM transform (cylinderMs ++ ms)
			asCairo baseSet

drawIt ifs doRender (DrawSet baseSet n) = do
	forM_ [1..10] $ \pot -> do
		let res = 2^(pot :: Integer) :: Integer
		let s = runIFS n ifs (asSet baseSet)
		pausingForM_ 1000 (range ((0,0), (res,res))) $ \(x',y') -> doRender $ do
				setSourceRGB 0 0 0
				setAntialias AntialiasGray
				let factor = 1/realToFrac res
				    x = factor * realToFrac x'
				    y = factor * realToFrac y'
				if s (x,y) then setSourceRGB 0 0 0
					   else setSourceRGB 1 1 1 
				rectangle x y factor factor
				fill

drawIt ifs doRender (DrawChaos num showLines) = do
	diceRolls <- randomRs (0,length ifs - 1) `fmap` liftIO getStdGen
	let points = scanl (\p pick -> transformPoint (ifs !! pick) p) (0,0) diceRolls
	    pairs = zip points (tail points)
	if showLines
	 then do
		pausingForM_ 100 (take (2^num) pairs) $ \((x,y),(x',y')) -> doRender $ do
			setAntialias AntialiasGray
			setLineWidth (1/1000)
			moveTo x y
			lineTo x' y'
			stroke
	 else do
		pausingForM_ 1000 (take (2^num) points) $ \(x,y) -> doRender $ do
			setSourceRGBA 0 0 0 1
			setAntialias AntialiasGray
			rectangle x y (1/1000) (1/1000) -- hopefully one pixel
			fill
	
pausingForM_ :: Monad m => Int -> [a] -> (a -> CoroutineT m ()) -> CoroutineT m ()
pausingForM_ period list action = pausing' 0 list
  where pausing' _ []     = return ()
        pausing' n (x:xs) = do action x
		               if n==period then pause >> pausing' 0     xs
		                            else          pausing' (n+1) xs


redraw ifs widget getRend = do
	(w',h') <- liftIO $ widgetGetSize widget
	Just win <- liftIO $ widgetGetWindow widget
	let w = realToFrac w'
	    h = realToFrac h'
	(renderer, showIFS, traceMB) <- liftIO $  getRend
	let doRender r = liftIO $ renderWithDrawWindow win (scale w h >> r)
	doRender (setSourceRGB 1 1 1 >> paint)
	when showIFS $ doRender $ drawIFS ifs -- render below, because its faster
	doMB traceMB $ drawTrace ifs doRender 
	doMB renderer $ \rend -> do
		drawIt ifs doRender rend
		when (showIFS) $ doRender $ drawIFS ifs
	  	

idleHandler :: IO Bool -> IO (IO ())
idleHandler handler = do
	idleHandlerRef <- newIORef Nothing
	let modHandler = do
		ret <- handler
		unless ret $ writeIORef idleHandlerRef Nothing
		return ret
	let restart = do
		ih <- readIORef idleHandlerRef
		when (ih == Nothing) $ do
			ref <- idleAdd modHandler priorityLow 
			writeIORef idleHandlerRef (Just ref)
	return restart
		
redrawHandler :: WidgetClass w => w -> IO (ScreenConfig) -> IO IFS -> IO (IO ())
redrawHandler canvas getRend getIFS = do 
	resumeRef <- newIORef Nothing 
	idleRestart <- idleHandler $ do
		resumeMB <- readIORef resumeRef
		case resumeMB of
			Nothing    -> return False
			(Just res) -> do ret <- runCoroutineT res
			                 writeIORef resumeRef ret
				         return (isJust ret)
	return $ do
		--writeIORef resRef 1
		ifs <- getIFS
		writeIORef resumeRef $ Just (redraw ifs canvas getRend)
		idleRestart
	

mouseHandler xml getTab ifsRef callback = do
	canvas <- builderGetObject xml castToDrawingArea "drawingarea"
	widgetAddEvents canvas [Button1MotionMask, KeyPressMask, KeyReleaseMask]
	dragRef <- newIORef (Nothing :: Maybe (Int, Bool, (Double, Double)))

	on canvas buttonPressEvent $ do
		tab <- liftIO $ getTab
                b <- eventButton
                (x,y) <- eventCoordinates
		liftIO $ case tab of
		  IFSTab -> when (b == LeftButton) $ do
			-- find a approximately selected point 
			(w',h') <- widgetGetSize canvas
			ifs <- readIORef ifsRef
                        let p = (x/realToFrac w', y/realToFrac h')
			    checkMatrix i = checkBasePoint i : checkArrowPoint i : []
		    	    checkBasePoint = checkPoint basePoint True
		    	    checkArrowPoint = checkPoint arrowTip False
		    	    checkPoint bp b i = 
			    	let bp' = transformPoint (ifs !! i) bp
			       	    di = dist p bp'
			    	    de = delta p bp'
			    	in (di, (i, b, de))
			writeIORef dragRef $
				selectPoint $ concatMap checkMatrix [0..length ifs-1]
		
		  CodingTab -> when (b == LeftButton) $ do
			(w',h') <- widgetGetSize canvas
			ifs <- readIORef ifsRef
			spinCodeLen <- builderGetObject xml castToSpinButton "spinCodeLen"
			codeLenght <- round `fmap` get spinCodeLen spinButtonValue
                        let p = (x/realToFrac w', y/realToFrac h')
			    code = ifsCode codeLenght ifs p
			    text = maybe "Nicht in der Menge" (concatMap show) code
			label <- builderGetObject xml castToLabel "labelCoding"
			set label [labelLabel := "Kodierung: " ++ text]
		
		  _ -> return ()

		return True

	on canvas buttonReleaseEvent $ liftIO $ writeIORef dragRef Nothing >> return True

	on canvas motionNotifyEvent $ do
                (ex, ey) <- eventCoordinates
                liftIO $ do
                    dragging <- readIORef dragRef
                    (w',h') <- widgetGetSize canvas
                    let (x,y) = (ex/realToFrac w', ey/realToFrac h')
                    case dragging of
                      Just (i,True, dp) -> do
                            -- moving the base point
                            ifs <- readIORef ifsRef
                            let (before,m:after) = splitAt i ifs
                                (bx,by) = transformPoint m basePoint
                                (nx,ny) = addDelta (x,y) dp
                                newM = CM.translate (nx-bx) (ny-by) m
                                newifs = before ++ newM : after
                            writeIORef ifsRef newifs
                            comboBox <- builderGetObject xml castToComboBox "comboIFS"
                            comboBoxSetActive comboBox 0
                            callback
                      Just (i,False, dp) -> do
                            -- moving the arrow tip
                            ifs <- readIORef ifsRef
                            let (before,m:after) = splitAt i ifs
                                (bx,by) = transformPoint m basePoint
                                (nx,ny) = addDelta (x,y) dp
                                newScale = min (max (2*(dist (nx,ny) (bx,by))) eps) 1
                                newRot = atan2 (ny-by) (nx-bx) + pi/2
                                newM = CM.translate (bx-1/2) (by-1/2) . scaleMiddle newScale .
                                            rotateMiddle newRot $ CM.identity
                                newifs = before ++ newM : after
                            writeIORef ifsRef newifs
                            comboBox <- builderGetObject xml castToComboBox "comboIFS"
                            comboBoxSetActive comboBox 0
                            callback
                      Nothing -> return ()
                    return True

  where eps = 1/10
	basePoint = (1/2, 1/2)
	arrowTip  = (1/2, 0)
        dist (x,y) (x',y') = sqrt ((x-x')**2 + (y-y')**2)
        delta (x,y) (x',y') = (x'-x, y'-y)
        addDelta (x,y) (dx,dy) = (x + dx, y + dy)
	selectPoint pts = let (d, c) = minimumBy fst pts
	                  in c `justIf` (d < eps)
	minBy f v1 v2 = if f v1 <= f v2 then v1 else v2
	minimumBy f = foldl1 (minBy f) 

	
handleCodeButton xml handler = do
	button <- builderGetObject xml castToButton "buttonCopyCode"
	on button buttonActivated $ liftIO $ do
		label <- builderGetObject xml castToLabel "labelCoding"
		labelText <- get label labelLabel
		let codeText = drop (length "Kodierung: ") labelText
		when (codeText /= "Nicht in der Menge") $ do
			entry <- builderGetObject xml castToEntry "entryCylinder"
			set entry [entryText := codeText]

setDefaults xml = do
	radio <- builderGetObject xml castToRadioButton "radioCairo"
	set radio [ toggleButtonActive := True]
	
	comboBox <- builderGetObject xml castToComboBox "comboBaseSet"
	comboBoxSetModelText comboBox
	comboBoxRemoveText comboBox 0
	mapM_ (comboBoxAppendText comboBox . T.pack . show)  [minBound .. maxBound :: BaseSet]
	comboBoxSetActive comboBox 0


data ActiveTab = RenderTab | IFSTab | CodingTab | SystemTab deriving (Eq, Enum)

getActiveTab xml = do
	notebook <- builderGetObject xml castToNotebook "notebook"
	active <- get notebook notebookPage
	return $ toEnum active

getRenderer xml getIFS = do
	tab <- getActiveTab xml

	showFrak <- getToggleActive "toggleShowFraktal"
	showTrace <- getToggleActive "toggleCylinderTrace"
	ifs <- getIFS
	traceMb  <- if   showTrace
	            then parseCylinder ifs `fmap` getEntryText "entryCylinder"
		    else return Nothing

	frakDrawer <- getRegularDraw
	case tab of
		IFSTab    -> return (frakDrawer `justIf` showFrak, True, Nothing)
		CodingTab -> return (Just frakDrawer, False, traceMb)
		-- RenderTab or System Tab:
		_         -> return (Just frakDrawer, False, Nothing)
	

  where choices = [ ("radioSet",   getSetRenderer)
                  , ("radioCairo", getCairoRenderer)
		  , ("radioChaos", getChaosRenderer)
		  ]
	getRegularDraw = fmap (fromJust . msum) $ forM choices $ \(w,r) -> do
		active <- getToggleActive w
		if active then Just `fmap` r else return Nothing
	getSetRenderer = do
		baseSet <- getComboSelection "comboBaseSet"
		value <- getSpinValue "spinSetDepth"
		return $ DrawSet (toEnum baseSet) value
	getCairoRenderer = do
		baseSet <- getComboSelection "comboBaseSet"
		value <- getSpinValue "spinCairoDepth"
		ifs <- getIFS
	        cylinder <- getEntryText "entryCylinder"
		return $ DrawCairo
			(toEnum baseSet)
			(fromMaybe [] $ parseCylinder ifs cylinder)
			value
	getChaosRenderer = do
		value <- getSpinValue "spinChaosPoints"
		showLines <- getToggleActive "toggleShowLines"
		return $ DrawChaos value showLines
	getSpinValue w = do 
		spin <- builderGetObject xml castToSpinButton w
		round `fmap` get spin spinButtonValue
	getToggleActive w = do 
		toggle <- builderGetObject xml castToToggleButton w
		get toggle toggleButtonActive
	getEntryText w = do 
		entry <- builderGetObject xml castToEntry w
		get entry entryText
	getComboSelection w = do 
		comboBox <- builderGetObject xml castToComboBox w
		max 0 <$> comboBoxGetActive comboBox
	parseCylinder ifs = do
		sequence . map (\n' -> do
			n <- readM [n']
			guard $ 0 <= n && n <= length ifs - 1
			return n)

	
onRendererChange xml getRend realHandler = do
	lastDrawRef <- getRend >>= newIORef

	let handler = do
		lastDraw <- readIORef lastDrawRef
		newDraw <- getRend
		when (lastDraw /= newDraw) $ do
			writeIORef lastDrawRef newDraw
			realHandler

	forM ["radioSet", "radioCairo", "radioChaos", "toggleCylinderTrace",
	      "toggleShowLines", "toggleShowFraktal"] $ \wn -> do
                w <- builderGetObject xml castToToggleButton wn
                on w toggled handler
	forM ["spinCairoDepth", "spinSetDepth", "spinChaosPoints"] $ \wn -> do
		w <- builderGetObject xml castToSpinButton wn
                onValueSpinned w handler
	forM ["entryCylinder"] $ \wn -> do
		w <- builderGetObject xml castToEntry wn
                on w editableChanged handler
	w <- builderGetObject xml castToNotebook "notebook"
        after w switchPage  (const handler)
	w <- builderGetObject xml castToComboBox "comboBaseSet"
        on w changed handler

ifsLabel xml ifs = do
	label <- builderGetObject xml castToLabel "labelIFS"
	set label [labelLabel := text]
  where matrixLines (Matrix a1 b1 a2 b2 c1 c2) = 
  		[ printf "/%5.2f %5.2f\\ _|_ /%5.2f\\" a1 b1 c1
		, printf "\\%5.2f %5.2f/  |  \\%5.2f/" a2 b2 c2
		]
	mlist = intercalate "\n" . map (intercalate "  ") . map (zipWith colorize ifsColors) .
			transpose . map matrixLines $ ifs
	text = "Aktuelles IFS:\n<tt>"++mlist++"</tt>"
	colorize :: (Double, Double, Double) -> String -> String
	colorize (r,g,b) s = "<span foreground=\""++hexcolor++"\">" ++ s ++ "</span>"
	  where hexcolor = printf "#%02X%02X%02X" (round' r) (round' g) (round' b)
	        round' :: Double -> Int
		round' d = round (d*255)

onIFSChange xml ifsRef realHandler = do
	comboBox <- builderGetObject xml castToComboBox "comboIFS"
	comboBoxSetModelText comboBox
	mapM_ (comboBoxAppendText comboBox . T.pack) $ "(custom IFS)" : map fst knownIFS

	spinNum <- builderGetObject xml castToSpinButton "spinNumPhi"

	let handler = do
		curifs <- readIORef ifsRef
		textMB <- fmap T.unpack <$> comboBoxGetActiveText comboBox
		doMB textMB $ \text -> do
                    doMB (lookup text knownIFS) $ \ifs -> do
                        writeIORef ifsRef ifs
                        set spinNum [spinButtonValue := fromIntegral (length ifs)]
                        realHandler
	on comboBox changed handler

	afterValueSpinned spinNum $ do
		curifs <- readIORef ifsRef
		n <- round `fmap` get spinNum spinButtonValue
		when (n /= length curifs) $ do
			let ifs = take n (curifs ++ repeat (scaleMiddle (1/2) CM.identity))
			writeIORef ifsRef ifs
			comboBoxSetActive comboBox 0
			realHandler

	comboBoxSetActive comboBox 1
	handler


ifsUpdated xml ifsRef = do
	ifs <- readIORef ifsRef
	ifsLabel xml ifs

handleFileButtons xml ifsRef handler = do
	saveButton <- builderGetObject xml castToButton "buttonSave"
	openButton <- builderGetObject xml castToButton "buttonOpen"
	window <- builderGetObject xml castToWindow "window"

	filter <- fileFilterNew
	fileFilterSetName filter "Faktal-Dateien"
	fileFilterAddPattern filter "*.frak"

	on saveButton buttonActivated $ do
		dialog <- fileChooserDialogNew
              			(Just $ "Fraktal speichern")
              			(Just window)
				FileChooserActionSave
				[("gtk-cancel",ResponseCancel)
				,("gtk-save" , ResponseAccept)]

		fileChooserAddFilter dialog filter
		widgetShow dialog
		response <- dialogRun dialog
		case response of
		  ResponseAccept -> do  fileNameMB <- fileChooserGetFilename dialog
					doMB fileNameMB $ \fileName -> do
						let fileName' =
							if ".frak" `isSuffixOf` fileName
							then fileName
							else fileName ++ ".frak"
						ifs <- readIORef ifsRef
						writeFile fileName' (show ifs)
		  _ -> return ()
		widgetDestroy dialog

	on openButton buttonActivated $ do
		dialog <- fileChooserDialogNew
              			(Just $ "Fraktal öffnen")
              			(Just window)
				FileChooserActionOpen
				[("gtk-cancel",ResponseCancel)
				,("gtk-open" , ResponseAccept)]

		fileChooserAddFilter dialog filter
		widgetShow dialog
		response <- dialogRun dialog
		case response of 
		  ResponseAccept -> do  fileNameMB <- fileChooserGetFilename dialog
		  			doMB fileNameMB $ \fileName -> do 
						content <- readFile fileName
						writeIORef ifsRef (read content)
		  _ -> return ()
		widgetDestroy dialog


main = do
	ifsRef <- newIORef sierpinsky


	initGUI

        xml <- builderNew
        builderAddFromFile xml "FrakView.xml"
	window <- builderGetObject xml castToWindow "window"
	canvas <- builderGetObject xml castToDrawingArea "drawingarea"

	setDefaults xml
	
	let getRend = getRenderer xml (readIORef ifsRef)

	restartDrawing <- redrawHandler canvas getRend (readIORef ifsRef)

	let restartIFSDrawing = ifsUpdated xml ifsRef >> restartDrawing

	on window keyPressEvent $ do
            val <- eventKeyName
            liftIO $ case () of
                () | val == T.pack "q" -> widgetDestroy window >> return True
                   | val == T.pack "f" -> windowFullscreen window >> return True
                   | otherwise         -> return False
	on window objectDestroy mainQuit
        quitButton <- builderGetObject xml castToButton "buttonquit"
	on quitButton buttonActivated $ liftIO $ widgetDestroy window

	handleCodeButton xml restartDrawing

	handleFileButtons xml ifsRef restartIFSDrawing

	onRendererChange xml getRend restartDrawing
	on canvas draw $ liftIO restartDrawing
	onIFSChange xml ifsRef restartIFSDrawing

	mouseHandler xml (getActiveTab xml) ifsRef $ do
		ifsUpdated xml ifsRef
		restartDrawing

	ifsUpdated xml ifsRef

	widgetShowAll window
	mainGUI


widgetGetSize widget = do
    w <- widgetGetAllocatedWidth widget
    h <- widgetGetAllocatedHeight widget
    return (w,h)
