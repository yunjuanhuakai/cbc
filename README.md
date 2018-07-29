# cbc

cbc的haskell实现
</br>
不支持 int a,b,c 形式的声明，只能单类型单变量声明
</br>
支持浮点数运算
</br>
struct变量类型不需要写struct关键字，union同理
</br>
声明文件与源码分离
</br>
开发ing
</br>

算术运算提升规则:
|        | bool    | char          | int           | float         | double        | long          | uchar         | uint          | ulong         | other   |
|--------|---------|---------------|---------------|---------------|---------------|---------------|---------------|---------------|---------------|---------|
| bool   | Nothing | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing |
| char   | Nothing | Just CbInt    | Just CbInt    | Just CbFloat  | Just CbDouble | Just CbLong   | Just CbInt    | Just CbUInt   | Just CbULong  | Nothing |
| int    | Nothing | Just CbInt    | Just CbInt    | Just CbFloat  | Just CbDouble | Just CbLong   | Just CbInt    | Just CbUInt   | Just CbULong  | Nothing |
| float  | Nothing | Just CbFloat  | Just CbFloat  | Just CbFloat  | Just CbDouble | Just CbFloat  | Just CbFloat  | Just CbFloat  | Just CbFloat  | Nothing |
| double | Nothing | Just CbDouble | Just CbDouble | Just CbDouble | Just CbDouble | Just CbDouble | Just CbDouble | Just CbDouble | Just CbDouble | Nothing |
| long   | Nothing | Just CbLong   | Just CbLong   | Just CbFloat  | Just CbDouble | Just CbLong   | Just CbULong  | Just CbULong  | Just CbULong  | Nothing |
| uchar  | Nothing | Just CbInt    | Just CbInt    | Just CbFloat  | Just CbDouble | Just CbInt    | Just CbUInt   | Just CbUInt   | Just CbULong  | Nothing |
| uint   | Nothing | Just CbUInt   | Just CbUInt   | Just CbFloat  | Just CbDouble | Just CbLong   | Just CbUInt   | Just CbUInt   | Just CbULong  | Nothing |
| ulong  | Nothing | Just CbULong  | Just CbULong  | Just CbFloat  | Just CbDouble | Just CbULong  | Just CbULong  | Just CbULong  | Just CbULong  | Nothing |
| other  | Nothing | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing       | Nothing |