module GenomRequest
where

data GenomSpec = GenomSpec {
	moduleName :: String,
	number :: Integer,
	version :: [Integer],
	email :: Maybe String,
	internalData :: String,
	requires :: [String],
	codel_requires :: [String]
} deriving (Show)

type FunctionName = String
type Type = String
type SDIRef = String
data Argument = Arg Type SDIRef deriving (Show)
data Prototype = Proto (Maybe Argument) (Maybe [(String, String)]) (Maybe Argument)
					deriving (Show)

input :: Prototype -> Maybe Argument
input (Proto input_ _ _) = input_

output :: Prototype -> Maybe Argument
output (Proto _ _ output_) = output_

type ErrorName = String
type ErrorList = Maybe [ErrorName]
type Doc = Maybe String

data ExecSpec = ExecSpec {
				task :: String,
				exec :: Maybe FunctionName,
				exec_ctrl :: Maybe FunctionName,
				exec_start :: Maybe FunctionName,
				exec_end :: Maybe FunctionName,
				exec_inter :: Maybe FunctionName,
				exec_fail :: Maybe FunctionName,
				exec_input :: Maybe String
			}
	deriving (Show)

data CtrlSpec = CtrlSpec {
				ctrl :: Maybe FunctionName,
				ctrl_input :: Maybe String
			}
	deriving (Show)

data Rqst = ExecRequest String Doc Prototype ExecSpec ErrorList
		   |InitRequest String Doc Prototype ExecSpec ErrorList
		   |CtrlRequest String Doc Prototype CtrlSpec ErrorList
	deriving (Show)

name :: Rqst -> String
name (ExecRequest name_ _ _ _ _) = name_
name (CtrlRequest name_ _ _ _ _) = name_
name (InitRequest name_ _ _ _ _) = name_

proto :: Rqst -> Prototype
proto (ExecRequest _ _ proto_ _ _) = proto_
proto (CtrlRequest _ _ proto_ _ _) = proto_
proto (InitRequest _ _ proto_ _ _) = proto_

execSpec :: Rqst -> Maybe ExecSpec
execSpec (ExecRequest _ _ _ exec_ _) = Just exec_
execSpec (InitRequest _ _ _ exec_ _) = Just exec_
execSpec (CtrlRequest _ _ _ _ _) = Nothing

ctrlSpec :: Rqst -> Maybe CtrlSpec 
ctrlSpec (ExecRequest _ _ _ _ _) = Nothing
ctrlSpec (InitRequest _ _ _ _ _) = Nothing
ctrlSpec (CtrlRequest _ _ _ ctrl _) = Just ctrl

errors :: Rqst -> ErrorList
errors (ExecRequest _ _ _ _ errors_) = errors_
errors (InitRequest _ _ _ _ errors_) = errors_
errors (CtrlRequest _ _ _ _ errors_) = errors_

data Poster = Poster String String (Maybe [Argument]) String
	deriving (Show)

posterName :: Poster -> String
posterName (Poster name _ _ _) = name

data Task = Task {
		taskName :: String,
		period :: Integer,
		priority :: Maybe Integer,
		delay :: Maybe Integer,
		stackSize :: Maybe Integer,
		initFunc :: Maybe FunctionName,
		endFunc :: Maybe FunctionName,
		func :: Maybe FunctionName,
		fails :: ErrorList
	} deriving (Show)

data GenomDescr = GenomDescr GenomSpec [Rqst] [Poster] [Task]
	deriving (Show)
