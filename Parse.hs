import Data.List
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Perm 
import Text.ParserCombinators.Parsec.Language
import Control.Applicative
import Control.Monad (MonadPlus(..), ap, liftM)
import GenomRequest

genomDef :: LanguageDef st
genomDef = javaStyle 
	{  
		reservedNames = ["request", "poster", "module"],
		caseSensitive = True
	}

genomLexer :: P.TokenParser st
genomLexer = P.makeTokenParser genomDef

-- XXX Not needed with Parsec3 which already implement it
-- The Applicative instance for every Monad looks like this.
instance Applicative (GenParser s a) where
     pure  = return
     (<*>) = ap

-- The Alternative instance for every MonadPlus looks like this.
instance Alternative (GenParser s a) where
	empty = mzero
	(<|>) = mplus

braces = P.braces genomLexer
quotedString = P.stringLiteral genomLexer
identifier = P.identifier genomLexer
natural = P.natural genomLexer
symbol str = P.symbol genomLexer str
colon = P.colon genomLexer
semi = P.semi genomLexer
comma = P.comma genomLexer
whiteSpace = P.whiteSpace genomLexer
quoted p = between (char '"') (char '"') p

versionParser' =  natural `sepBy` (char '.')
versionParser = quoted versionParser'

requiresParser = requires `sepBy` comma
	where 
		requires = identifier <|> quotedString

genomPair str p = symbol str *> colon *> p <* semi

genomFixedPair str fix = genomPair str $ symbol fix

optionalGenomPair str p = (Nothing, Just `liftA` (try $ genomPair str p))

infoParser = info `sepBy` comma
	where 
		info = liftA2 (,) (many $ noneOf ":") (string "::" *> quotedString) 

symbolList = identifier `sepBy` comma

inOutParser = liftA2 (,) identifier (string "::" *> identifier)

genomStruct keyword p =  do
	name <- (symbol keyword *> identifier) 
	res <-  ((braces $ p name) <* semi)
	return $ res

mkArg :: (Type, SDIRef) -> Argument
mkArg (t, s) = Arg t s

genomSpecParser = genomStruct "module" specParser 
	where
		specParser name = permute ( GenomSpec name  
						 <$$> (try $ genomPair "number" natural)
						 <||> (try $ genomPair "version" versionParser)
						 <|?> (optionalGenomPair "email" quotedString)
						 <||> (try $ genomPair "internal_data" identifier)
						 <||> (try $ genomPair "requires" requiresParser)
						 <||> (try $ genomPair "codels_requires" requiresParser)
								 )

rqstExecParser type_ name  = permute ( mkRequest name
				<$$> (try $ genomFixedPair "type" type_)
				<||> (try $ genomPair "exec_task" identifier)
				<|?> (optionalGenomPair "doc" quotedString)
				<|?> (optionalGenomPair "input" inOutParser)
				<|?> (optionalGenomPair "input_info" infoParser)
				<|?> (optionalGenomPair "output" inOutParser)
				<|?> (optionalGenomPair "c_control_func" identifier)
				<|?> (optionalGenomPair "c_exec_func_start" identifier)
				<|?> (optionalGenomPair "c_exec_func" identifier)
				<|?> (optionalGenomPair "c_exec_func_end" identifier)
				<|?> (optionalGenomPair "c_exec_func_inter" identifier)
				<|?> (optionalGenomPair "c_exec_func_fail" identifier)
				<|?> (optionalGenomPair "posters_input" identifier)
				<|?> (optionalGenomPair "fail_msg" symbolList)
				<|?> (optionalGenomPair "incompatible_with" symbolList)
				)
	where mkRequest name kind task doc in_ ininfo out 
					ctrl exec_st exec exec_end exec_inter exec_fail p_input
					fails incompatible =
				let prot = Proto (liftM mkArg in_) ininfo (liftM mkArg out);
					spec = ExecSpec task exec ctrl exec_st exec_end exec_inter 
						   exec_fail p_input
					in
				case kind of "exec" -> ExecRequest name doc prot spec fails;
							 "init" -> InitRequest name doc prot spec fails;

genomRqstExecParser = genomStruct "request" $ rqstExecParser "exec"

genomRqstInitParser = genomStruct "request" $ rqstExecParser "init"

genomRqstCtrlParser = genomStruct "request" rqstCtrlParser
	where
		mkCtrlRequest name kind doc in_ ininfo out ctrl poster fails incompatible =
				CtrlRequest name doc 
								 (Proto (liftM mkArg in_) ininfo (liftM mkArg out))
								 (CtrlSpec ctrl poster)
								 fails
		rqstCtrlParser name = permute ( mkCtrlRequest name
							<$$> (try $ genomFixedPair "type" "control")
							<|?> (optionalGenomPair "doc" quotedString)
							<|?> (optionalGenomPair "input" inOutParser)
							<|?> (optionalGenomPair "input_info" infoParser)
							<|?> (optionalGenomPair "output" inOutParser)
							<|?> (optionalGenomPair "c_control_func" identifier)
							<|?> (optionalGenomPair "posters_input" identifier)
							<|?> (optionalGenomPair "fail_msg" symbolList)
							<|?> (optionalGenomPair "incompatible_with" symbolList)
									  )

rqstParser = many $ choice [try genomRqstExecParser, try genomRqstCtrlParser, 
					 try genomRqstInitParser]

posterParser = genomStruct "poster" parser
	where 
		dataParser = inOutParser `sepBy` comma
		mkPoster name kind datas task = 
				Poster name kind (liftM (map mkArg) datas) task
		parser name = permute ( mkPoster name
						  <$$> (try $ genomPair "update" identifier)
						  <|?> (optionalGenomPair "data" dataParser)
						  <||> (try $ genomPair "exec_task" identifier)
						  )

taskParser = genomStruct "exec_task" parser
	where
		parser name = permute (Task name
						<$$> (try $ genomPair "period" natural)
						<|?> (optionalGenomPair "delay" natural)
						<|?> (optionalGenomPair "priority" natural)
						<|?> (optionalGenomPair "stack_size" natural)
						<|?> (optionalGenomPair "c_init_func" identifier)
						<|?> (optionalGenomPair "c_end_func" identifier)
						<|?> (optionalGenomPair "c_func" identifier)
						<|?> (optionalGenomPair "fail_msg" symbolList)
						)

genomParser = GenomDescr <$> (whiteSpace *> genomSpecParser)
						 <*> rqstParser
						 <*> (many posterParser)
						 <*> (many taskParser)
				
testParse path = fmap (parse genomParser "") $ readFile path
