
module Config (
    maxchar, max_list_len, max_tuple_len, max_array_len, max_vnts,
    max_flds,
    line_len, one, apply_prec,
    constructor_name, type_name, val_name,
    and_name, div_name, drop_name, eq_name, int_to_char_name, less_name,
    minus_name, mult_name, negate_name, not_name,
    or_name, plus_name, print_name, take_name, assoc_name,
    array_type_name, bool_type_name, char_type_name, double_type_name,
    int_type_name, integer_type_name, float_type_name,
    list_separator, field_separator, empty_string, val_def, type_def,
    union,
    lbrack, rbrack, lbrace, rbrace, lsq, rsq, space, newline,
    data_name, mod_name, main_name, where_name, decl_sep, derive_eq,
    lambda_name, map_name, array_name,
    file_ext,
    version
    ) where

maxchar ::  Int
maxchar  =  127

max_list_len ::  Int
max_list_len  =  5

max_tuple_len ::  Int
max_tuple_len  =  4

max_array_len ::  Int
max_array_len  =  5

max_vnts ::  Int
max_vnts  =  6

max_flds ::  Int
max_flds  =  4

line_len ::  Int
line_len  =  70

one :: Int
one  =  1

apply_prec :: Int
apply_prec  =  10

constructor_name, type_name, val_name :: String
constructor_name  =  "Cons"
type_name         =  "Type"
val_name          =  "val"

and_name, div_name, drop_name, eq_name, int_to_char_name, less_name,
    minus_name, mult_name, negate_name, not_name, or_name, plus_name,
    print_name, take_name, assoc_name :: String
and_name          =  "&&"
div_name          =  "div"
drop_name         =  "drop"
eq_name           =  "=="
int_to_char_name  =  "chr"
less_name         =  "<"
minus_name        =  "-"
mult_name         =  "*"
negate_name       =  "negate"
not_name          =  "not"
or_name           =  "||"
plus_name         =  "+"
print_name        =  "print"
take_name         =  "take"
assoc_name        =  ":="

array_type_name, bool_type_name, char_type_name, double_type_name,
    int_type_name, integer_type_name, float_type_name :: ShowS
array_type_name    =  showString "Array"
bool_type_name     =  showString "Bool"
char_type_name     =  showString "Char"
double_type_name   =  showString "Double"
int_type_name      =  showString "Int"
integer_type_name  =  showString "Integer"
float_type_name    =  showString "Float"

list_separator, field_separator, empty_string, val_def, type_def,
    union :: ShowS
list_separator   =  showString ", "
field_separator  =  showString " "
empty_string     =  showString ""
val_def          =  showString " = "
type_def         =  showString " = "
union            =  showString " | "

lbrack, rbrack, lbrace, rbrace, lsq, rsq, space, newline :: ShowS
lbrack   =  showChar '('
rbrack   =  showChar ')'
lbrace   =  showChar '{'
rbrace   =  showChar '}'
lsq      =  showChar '['
rsq      =  showChar ']'
space    =  showChar ' '
newline  =  showChar '\n'

data_name, mod_name, main_name, decl_sep, derive_eq, lambda_name,
    map_name, array_name :: ShowS
data_name    =  showString "data"
mod_name     =  showString "module"
main_name    =  showString "main"
where_name   =  showString "where"
decl_sep     =  showString ";"
derive_eq    =  showString "deriving (Eq)"
lambda_name  =  showString "\\"
map_name     =  showString "->"
array_name   =  showString "array"

file_ext :: String
file_ext  =  ".hs"

version :: ShowS
version  =  showString "1.20 dated 92/07/20"
