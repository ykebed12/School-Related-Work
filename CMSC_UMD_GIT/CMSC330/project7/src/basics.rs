/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 2 && n >= 0 { return 1 }
    else if n < 0 { return -1}
    else {
        let x = gauss(n-1);
        return n + x 
    }
    
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut count = 0;
    for elem in ls.iter() {
        if *elem >= s && *elem <= e {
            count += 1;
        }

    }
    return count
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    for elem in target.iter() {
        if !set.contains(&elem) {
            return false
        }
    }
    return true
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    let mut count = 0.0;
    let mut sum = 0.0;

    for elem in ls.iter() {
        sum += elem;
        count += 1.0;
    }
    
    if count == 0.0 {
        return None
    }else {
        return Some(sum/count);
    }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {

    let mut size = (*ls).len();
    let base: i32 = 2;
    let mut num = 0;
    for elem in (*ls).iter() {
        size -= 1;
        num += base.pow(size as u32) * elem;
    }
    return num;
}


/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut xs = vec![];
    let mut m = n;

    while m % 2 == 0 { 
        xs.push(2); 
        m /= 2; 
    } 

    for i in 3..((m/2)+1) { 
 
        while m % i == 0 { 
            xs.push(i);
            m = m/i;
        } 
    }

    if m > 2 {
        xs.push(m);
    }
    
    return xs;
}


/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {

    if lst.is_empty() {
        return vec![];
    } else if lst.len() == 1 {
        return lst.to_vec();
    } else {
        let mut vec = vec![];
        for i in 1..(lst.len()) {
            vec.push(lst[i]);
        }
        let last = lst[0];
        let mut b = vec![last];
        vec.append(&mut b);
        return vec;
        
    }

}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {

    for i in 0..(s.len()) {
        for j in 0..(target.len()) {
            if &s[i+j] != target[j] {
                break;
            }
            if j == target.len() - 1 {
                return true;
            }

        }
    }

    return false;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s.len() == 0 {
      return None
    } else if s.len() == 1 {
        return Some(s);
    }
      let mut sum = 1;
      let mut temp_max = 0;
      let mut max = 0;
      for i in 1..s.len() {
          if &s[i..i+1] == &s[i-1..i]{
              sum+=1;
          } else {
              sum = 1;
          }
          
          if sum > temp_max{
              temp_max = sum;
              max = i;
          }
      }
      let a = max+1 - temp_max;
      Some(&s[a..max+1])
  }
