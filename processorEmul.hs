import Data.Maybe ( catMaybes, fromMaybe )

-- v v v -- Do not change these types -- v v v --

data Memory a = Memory Integer [a] [a] deriving (Show, Eq)

type Value = Integer
data Output = S String | N Value deriving (Show, Eq)
data AddressType = Relative | Absolute deriving (Show, Eq)

data Condition = Neg | Zero | Pos deriving (Show, Eq)

data Instruction = Move RegisterName RegisterName
                 | Assign RegisterName Value
                 | Add RegisterName RegisterName
                 | Negate RegisterName

                 | Load RegisterName
                 | Store RegisterName
                 | Focus AddressType RegisterName

                 | Jump AddressType Integer
                 | JumpIf Condition RegisterName AddressType Integer

                 | Out RegisterName
                 | Trace String

                 | Halt
                 deriving (Show, Eq)


data RegisterName = R1 | R2 | R3 deriving (Show, Eq)
data Registers = Registers Value Value Value deriving (Show, Eq)

type Program = Memory Instruction
type Data = Memory Value

-- ^ ^ ^ -- Do not change these types -- ^ ^ ^ --

-- ------------------------------------------------------------------------- --
--                               YOUR SOLUTION                               --
-- ------------------------------------------------------------------------- --


getRegister :: Registers -> RegisterName -> Value
getRegister (Registers r1 _ _) R1 = r1
getRegister (Registers _ r2 _) R2 = r2
getRegister (Registers _ _ r3) R3 = r3


setRegister :: Registers -> RegisterName -> Value -> Registers
setRegister (Registers _ r2 r3) R1 value = Registers value r2 r3
setRegister (Registers r1 _ r3) R2 value = Registers r1 value r3 
setRegister (Registers r1 r2 _) R3 value = Registers r1 r2 value 
                                    

fromList :: [a] -> Memory a
fromList = Memory 0 [] 


fromListInf :: a -> [a] -> Memory a
fromListInf def_val xs = Memory 0 (repeat def_val) (xs ++ repeat def_val)


focusRel :: Integer -> Memory a -> Memory a
focusRel value memory @ (Memory pointer _ _) = focusAbs (pointer + value) memory


focusAbs :: Integer -> Memory a -> Memory a
focusAbs value (Memory pointer back forw) 
                | value == pointer = Memory pointer back forw
                | value > pointer = setMemory value (Memory pointer back forw) True 
                | otherwise = setMemory value (Memory pointer back forw) False 
            

setMemory :: Value -> Memory a -> Bool -> Memory a
setMemory value_ memory @ (Memory pointer_ xs_  ys_) forward
                    | value_ == pointer_ = memory
                    | null ys_ && forward = error "List index out of range."
                    | forward = setMemory value_ (Memory (pointer_ + 1) (head ys_ : xs_) (tail ys_)) forward
                    | null xs_ = error "List index out of range."
                    | otherwise = setMemory value_ (Memory (pointer_ - 1) (tail xs_) (head xs_ : ys_)) forward


evalStep :: Program -> Data -> Registers -> (Maybe Output, Program, Data, Registers)

evalStep instructions @ (Memory _ _ []) data_memory registers = (Nothing , instructions , data_memory , registers)


evalStep instructions @ (Memory _ _ (inst : _)) data_memory @ (Memory pointer back forw) registers = case inst of 
                        Halt -> (Nothing , instructions , data_memory , registers)                                       
                                                            
                        (Assign reg_name value) -> (Nothing , focusRel 1 instructions , data_memory , setRegister registers reg_name value)
                        (Move reg_name1 reg_name2) -> (Nothing , focusRel 1 instructions , data_memory , setRegister registers reg_name1 $ getRegister registers reg_name2)
                        (Add reg_name1 reg_name2) -> (Nothing , focusRel 1 instructions , data_memory , setRegister registers reg_name1 $ getRegister registers reg_name1 + getRegister registers reg_name2)
                        (Negate reg_name) -> (Nothing , focusRel 1 instructions , data_memory , setRegister registers reg_name $ negate $ getRegister registers reg_name)

                        (Load reg_name) -> (Nothing , focusRel 1 instructions , data_memory , setRegister registers reg_name (head forw))
                        
                        (Store reg_name) -> case forw of [] -> (Nothing , focusRel 1 instructions , Memory pointer back [getRegister registers reg_name] , registers)
                                                         (_:_) -> (Nothing , focusRel 1 instructions , Memory pointer back (getRegister registers reg_name : tail forw) , registers)
                            
                            
                        (Focus addr_type reg_name) -> case addr_type of Relative -> (Nothing , focusRel 1 instructions , focusRel (getRegister registers reg_name) data_memory , registers)
                                                                        Absolute -> (Nothing , focusRel 1 instructions , focusAbs (getRegister registers reg_name) data_memory , registers)


                        (Jump addr_type value) -> case addr_type of Relative -> (Nothing , focusRel value instructions , data_memory , registers) 
                                                                    Absolute -> (Nothing , focusAbs value instructions , data_memory , registers)

                        (JumpIf cond_type reg_name addr_type value) -> (if condCheck cond_type reg_name registers then (case addr_type of Relative -> (Nothing, focusRel value instructions, data_memory, registers)
                                                                                                                                          Absolute -> (Nothing, focusAbs value instructions, data_memory, registers))
                                                                                                                  else (Nothing, focusRel 1 instructions, data_memory, registers))

                                                                                               
                        (Out reg_name) -> (Just $ N $ getRegister registers reg_name , focusRel 1 instructions , data_memory , registers)    
                        (Trace text) -> (Just $ S text , focusRel 1 instructions , data_memory , registers)


                 , t_focusRel      True
                 , t_focusAbs      True
                 , t_getRegister   True
          


condCheck :: Condition -> RegisterName -> Registers -> Bool 
condCheck Neg reg_name_ registers_ = getRegister registers_ reg_name_ < 0 
condCheck Zero reg_name_ registers_ = getRegister registers_ reg_name_ == 0 
condCheck Pos reg_name_ registers_ = getRegister registers_ reg_name_ > 0 


evaluate :: (Maybe Output, Program, Data, Registers) -> [Maybe Output]
evaluate (output , Memory _ _ [] , _ , _) = [output]
evaluate (output , Memory _ _ (Halt : _) , _ , _) = [output]
evaluate (output , program , datas , registers) =  output : evaluate (evalStep program datas registers) 


eval :: [Instruction] -> [Value] -> [Output]
eval instructions values = catMaybes $ evaluate $ (Nothing , fromList instructions , fromListInf 0 values , Registers 0 0 0)
