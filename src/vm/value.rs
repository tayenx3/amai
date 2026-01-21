use super::arena::Arena;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Value(pub u64);

#[allow(unused)]
impl Value {
    #[inline(always)]
    pub fn from_int(x: i64) -> Self {
        Self(x as u64)
    }
    #[inline(always)]
    pub fn to_int(&self) -> i64 {
        self.0 as i64
    }

    #[inline(always)]
    pub fn from_bool(x: bool) -> Self {
        Self(x as u64)
    }
    #[inline(always)]
    pub fn to_bool(&self) -> bool {
        self.0 != 0
    }

    #[inline(always)]
    pub fn nil() -> Self {
        Self(0x0)
    }

    #[inline(always)]
    pub fn from_float(x: f64) -> Self {
        Self(x.to_bits())
    }
    #[inline(always)]
    pub fn to_float(&self) -> f64 {
        f64::from_bits(self.0)
    }
    
    #[inline(always)]
    pub fn from_ptr(addr: usize) -> Self {
        Self(addr as u64)
    }
    #[inline(always)]
    pub fn to_ptr(&self) -> usize {
        self.0 as usize
    }

    #[inline(always)]
    pub fn iadd(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        Ok(Self::from_int(lhs.checked_add(rhs).ok_or(format!("Addition overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn isub(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        Ok(Self::from_int(lhs.checked_sub(rhs).ok_or(format!("Subtraction overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn imul(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        Ok(Self::from_int(self.to_int().checked_mul(other.to_int()).ok_or(format!("Multiplication overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn idiv(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        if rhs == 0 { return Err(String::from("Division by zero (left: {lhs}, right: {rhs})")) }
        Ok(Self::from_int(lhs.checked_div(rhs).ok_or(format!("Division overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn irem(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        if rhs == 0 { return Err(format!("Division by zero (left: {lhs}, right: {rhs})")) }
        Ok(Self::from_int(lhs.checked_rem(rhs).ok_or(format!("Remainder overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn fadd(&self, other: Self) -> Self {
        Self::from_float(self.to_float() + other.to_float())
    }
    #[inline(always)]
    pub fn fsub(&self, other: Self) -> Self {
        Self::from_float(self.to_float() - other.to_float())
    }
    #[inline(always)]
    pub fn fmul(&self, other: Self) -> Self {
        Self::from_float(self.to_float() * other.to_float())
    }
    #[inline(always)]
    pub fn fdiv(&self, other: Self) -> Option<Self> {
        let o = other.to_float();
        if o == 0.0 { return None }
        Some(Self::from_float(self.to_float() / o))
    }
    #[inline(always)]
    pub fn frem(&self, other: Self) -> Option<Self> {
        let o = other.to_float();
        if o == 0.0 { return None }
        Some(Self::from_float(self.to_float() % o))
    }
    #[inline(always)]
    pub fn bor(&self, other: Self) -> Self {
        Self::from_int(self.to_int() | other.to_int())
    }
    #[inline(always)]
    pub fn band(&self, other: Self) -> Self {
        Self::from_int(self.to_int() & other.to_int())
    }
    #[inline(always)]
    pub fn bxor(&self, other: Self) -> Self {
        Self::from_int(self.to_int() ^ other.to_int())
    }
    #[inline(always)]
    pub fn bnot(&self) -> Self {
        Self::from_int(!self.to_int())
    }
    #[inline(always)]
    pub fn lor(&self, other: Self) -> Self {
        Self::from_bool(self.to_bool() || other.to_bool())
    }
    #[inline(always)]
    pub fn land(&self, other: Self) -> Self {
        Self::from_bool(self.to_bool() && other.to_bool())
    }
    #[inline(always)]
    pub fn lnot(&self) -> Self {
        Self::from_bool(!self.to_bool())
    }
    #[inline(always)]
    pub fn cmeq(&self, other: Self) -> Self {
        Self::from_bool(self.0 == other.0)
    }
    #[inline(always)]
    pub fn cmne(&self, other: Self) -> Self {
        Self::from_bool(self.0 != other.0)
    }
    #[inline(always)]
    pub fn icgt(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() > other.to_int())
    }
    #[inline(always)]
    pub fn iclt(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() < other.to_int())
    }
    #[inline(always)]
    pub fn icge(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() >= other.to_int())
    }
    #[inline(always)]
    pub fn icle(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() <= other.to_int())
    }
    #[inline(always)]
    pub fn fcgt(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() > other.to_float())
    }
    #[inline(always)]
    pub fn fclt(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() < other.to_float())
    }
    #[inline(always)]
    pub fn fcge(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() >= other.to_float())
    }
    #[inline(always)]
    pub fn fcle(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() <= other.to_float())
    }
    #[inline(always)]
    pub fn ineg(&self) -> Self {
        Self::from_int(-self.to_int())
    }
    #[inline(always)]
    pub fn fneg(&self) -> Self {
        Self::from_float(-self.to_float())
    }
    #[inline(always)]
    pub fn lshf(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        if rhs < 0 {
            return Err(format!("Shift left by negative integer (left: {lhs}, right: {rhs})"))
        }
        Ok(Self::from_int(lhs.checked_shl(rhs as u32).ok_or(format!("Shift left overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn rshf(&self, other: Self) -> Result<Self, String> {
        let lhs = self.to_int();
        let rhs = other.to_int();
        if rhs < 0 {
            return Err(format!("Shift right by negative integer (left: {lhs}, right: {rhs})"))
        }
        Ok(Self::from_int(lhs.checked_shr(rhs as u32).ok_or(format!("Shift right overflow (left: {lhs}, right: {rhs})"))?))
    }
    #[inline(always)]
    pub fn scon(&self, other: Self, arena: &mut Arena) -> Result<Self, String> {
        let lhs_addr = self.0 as usize;
        let rhs_addr = other.0 as usize;
        let lhs_size = u32::from_le_bytes(
            arena.fetch(lhs_addr, 4)
            .try_into()
            .unwrap()
        ) as usize;
        let rhs_size = u32::from_le_bytes(
            arena.fetch(rhs_addr, 4)
            .try_into()
            .unwrap()
        ) as usize;
        let new_len = lhs_size + rhs_size;
        let addr = arena.alloc(new_len + 4, 1);
        arena.write(addr, &new_len.to_le_bytes());
        let lhs_data = arena.fetch(lhs_addr + 4, lhs_size).to_vec();
        arena.write(addr + 4, &lhs_data);
        let rhs_data = arena.fetch(rhs_addr + 4, rhs_size).to_vec();
        arena.write(addr + 4 + lhs_size, &rhs_data);
        Ok(Self::from_ptr(addr))
    }
}