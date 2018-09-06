module Analysis.Alias where

import Analysis.Base

-- 别名以语句为单位，既需要实现的流函数及格的运算是在语句级别处理的
-- p = nil|init -> ptr(stmt, p) = ∅
-- p = alloc    -> ptr(stmt, p) = anon
-- 空对象和动态内存空间不影响别名信息
-- p = &a       -> ptr(stmt, p) = { var(stmt, a) }
-- p1 = p2      -> ptr(stmt, p1) = ptr(stmt, p2)
-- *p = a       -> if (type(ptr(p)) != ptr) then 无别名信息变化 else ptr(*p) = ptr(a)
-- p1 == p2     -> 在then块中，ptr(stmt, p1) = ptr(stmt, p2) = ptr'(stmt, p1) ∩ ptr'(stmt, p2)
-- 其余语句不会产生别名关系
-- 结构体变量覆盖其所有域，且任意两个域之间不会重叠
