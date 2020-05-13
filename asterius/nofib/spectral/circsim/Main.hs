
import System.Environment
import Control.Monad (forM_)
import Data.List

data BinTree a b = Cell a

put :: [a] -> BinTree a ()
put [x] = Cell x
put xs  = Node () (put fstHalf) (put sndHalf)

get :: BinTree a b -> [a]
get (Cell x) = [x]
get (Node x l r) = get l ++ get r

upsweep :: (a -> a -> a)		-- node function

upsweep f (Cell a)     = (a, Cell a)
upsweep f (Node x l r) = (f lv rv, Node (lv, rv) l' r')
    where

downsweep :: (a -> b -> c -> (c,c))	-- downsweep function

downsweep g d (Cell x) 	   = Cell d
downsweep g d (Node (lv,rv) l r) = Node () l' r'

sweep_ud :: (a -> a -> a)			-- upsweep node function

sweep_ud up down u t

scanL :: (a -> a -> a) -> a -> [a] -> (a,[a])
scanL f u xs = (up_ans, get t')

scanR :: (a -> a -> a) -> a -> [a] -> (a,[a])
scanR f u xs = (up_ans, get t')

scanlr :: (a -> a -> a)			-- left to right function

scanlr f g lu ru xs = (ans, get t)
    where
      ((l_ans,r_ans), t) = sweep_ud up down (lu,ru) (put xs')
      ans = (g r_ans ru, f lu l_ans)
      xs' = map (\x -> (x,x)) xs
      up (lx,ly) (rx,ry) = (f lx rx, g ly ry)
      down (lx,ly) (rx,ry) (a,b) = ((a, g ry b), (f a lx, b))

type Circuit a = 	(Int,		-- circuit size

type Label = (String, Pid)

type Pid = Int

data Component

data State a = PS
  {pid        :: Int,		-- site identifier
   compType   :: Component,	-- component represented in the site
   pathDepth  :: Int,		-- path depth at which outputs become valid
   inports    :: [InPort a],	-- tags and latches for the inputs
   outports   :: [OutPort a]	-- tags and latches for the outputs
  }

type InPort a =
  (Pid,	-- identifies processor that will supply the input signal
   Int,	-- the output port number of the signal
   a)		-- latch to hold the input signal value

type OutPort a =

{-

instance Show a => Show (State a) where

-}

instance Show a => Show (State a) where

{-
instance (Show a, Show b, Show c, Show d, Show e, Show f)

    showsPrec p (a,b,c,d,e,f) = showChar '(' . shows a . showChar ',' .

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)

    showsPrec p (a,b,c,d,e,f,g,h) = showChar '(' . shows a . showChar ',' .

-}

nearest_power_of_two :: Int -> Int
nearest_power_of_two x = until (>=x) (*2) 1

pad_circuit :: Signal a => Circuit a -> Circuit a
pad_circuit (size, ins, outs, states)

    where

emptyState :: Signal a => State a
emptyState = PS {	pid = -1,

class (Eq a, Show a) => Signal a where









data Boolean = F | T

instance Signal Boolean where

type Packet a =

emptyPacket :: Signal a => Packet a
emptyPacket = (-1, -1, zeroS, False, 0, False, 0, 1)

showPacket :: Signal a => Packet a -> IO ()
showPacket (pid, reqL, distL, reqR, distR, sender, msg, ext)

showPackets :: Signal a => [Packet a] -> IO ()
showPackets []     = putStr ""
showPackets (x:xs) = showPacket x >> showPackets xs

send_right :: Packet a -> Packet a -> Packet a
send_right (ia,sa,ma,qla,dla,qra,dra,ea) (ib,sb,mb,qlb,dlb,qrb,drb,eb) =

send_left :: Packet a -> Packet a -> Packet a
send_left (ia,sa,ma,qla,dla,qra,dra,ea) (ib,sb,mb,qlb,dlb,qrb,drb,eb) =

send :: Signal a => [Packet a]

send xs = scanlr send_right send_left emptyPacket emptyPacket xs

circuit_simulate :: Signal a => [[a]] -> Circuit a -> [[a]]
circuit_simulate inputs_list circuit

collect_outputs :: Circuit a -> [a]
collect_outputs (size, ins, outs, states) = map get_output outs
   where

simulate :: Signal a => [[a]] -> Circuit a -> [Circuit a]
simulate inputs_list circuit@(size, ins, outs, states)

    where

do_cycle :: Signal a => Int -> Circuit a -> [a] -> Circuit a
do_cycle cpd (size, ins, outs, states) inputs = (size, ins, outs, states4)
    where

restore_requests :: Signal a => [State a] -> [State a] -> [State a]
restore_requests old_states new_states

     where

do_sends :: Signal a => Int -> [State a] -> [State a]
do_sends d states = until (acknowledge d) (do_send d) states

acknowledge :: Signal a => Int -> [State a] -> Bool
acknowledge d states = not (or (map (check_requests . outports) states1))
    where

do_send :: Signal a => Int -> [State a] -> [State a]
do_send d states = zipWith (update_io d) pss' states
    where

update_io :: Signal a => Int -> [(Packet a,Packet a)] -> State a -> State a
update_io d lrps state = update_os (update_is state)
    where

update_o :: Signal a => (Packet a, Packet a) -> OutPort a -> OutPort a
update_o (lp, rp) out = check_left lp (check_right rp out)

check_left (pid, port, pm, pql, pdl, pqr, pdr, e) (p, m, ql, dl, qr, dr)

check_right (pid, port, pm, pql, pdl, pqr, pdr, e) (p, m, ql, dl, qr, dr)

update_i :: Signal a => (Packet a, Packet a) -> [InPort a] -> [InPort a]
update_i (l,r) ins = up_i l (up_i r ins)

up_i :: Signal a => Packet a -> [InPort a] -> [InPort a]
up_i (i, p, m', _, _, _, _, _) ins

compare_and_update :: Signal a => InPort a -> InPort a -> InPort a
compare_and_update (i, p, m') (pid, port, m)

make_packet :: Signal a => State a -> [Packet a]
make_packet state = [ (pid state, p, m, ql, dl, qr, dr, 1)

pad_packets :: Signal a => [[Packet a]] -> [[Packet a]]
pad_packets pss = map pad pss
    where

check_depth :: Signal a => Int -> State a -> State a
check_depth d state

update_requests :: Signal a => Bool -> State a -> State a
update_requests b state

simulate_components :: Signal a => Int -> [State a] -> [State a]
simulate_components depth states

simulate_component :: Signal a => Int -> State a -> State a
simulate_component d state = if d == pathDepth state && new_value/=Nothing

    where

apply_component :: Signal a => Component -> [a] -> Maybe a
apply_component Inp _      = Nothing
apply_component Outp [x]   = Just x
apply_component Dff [x]    = Just x
apply_component Inv [x]    = Just (inv x)
apply_component And2 [x,y] = Just (and2 x y)
apply_component Or2 [x,y]  = Just (or2 x y)
apply_component Xor [x,y]  = Just (xor x y)
apply_component _ _        = error "Error: apply_component\n"

store_inputs :: Signal a => [(Label,a)] -> State a -> State a
store_inputs label_inputs state@(PS {compType=Inp})

store_inputs label_inputs state = state

init_dffs :: Signal a => State a -> State a
init_dffs state = if compType state == Dff

critical_path_depth :: Signal a => Circuit a -> Int
critical_path_depth (size, ins, outs, states)

input_values :: Signal a => Int -> [[a]]
input_values nbits = map binary [0..2^nbits-1]
    where

update_outports :: Signal a => State a -> a -> State a
update_outports state value

    where

regs :: Signal a => Int -> Circuit a
regs bits = (size, is, os, states)
    where

reg :: Signal a => Pid -> Pid -> [State a]
reg sto n
  = [ PS { pid       = n,	-- x input -------------------------------------

main :: IO ()
main = forM_ [1..97] $ const $ do
  (num_bits:num_cycles:_) <- getArgs
  print (run (read num_bits) (read num_cycles))

run :: Int -> Int -> [[Boolean]]
run num_bits num_cycles = circuit_simulate cycles example
