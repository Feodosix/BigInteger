#include <algorithm>
#include <cmath>
#include <compare>
#include <string>
#include <vector>
#include <iostream>

// CLASS BIGINTEGER

class BigInteger {
 public:
  BigInteger(): sign_(sign::ZERO) {};
  BigInteger(long long num);
  BigInteger(const std::string& str);
  BigInteger(const BigInteger& other) = default;
  ~BigInteger() = default;

  BigInteger& operator=(const BigInteger& other) = default;

  BigInteger& operator+=(const BigInteger& other);
  BigInteger& operator-=(const BigInteger& other);
  BigInteger& operator*=(const BigInteger& other);
  BigInteger& operator/=(const BigInteger& other);
  BigInteger& operator%=(const BigInteger& other);

  BigInteger operator-() const;
  BigInteger& operator++();
  BigInteger operator++(int);
  BigInteger& operator--();
  BigInteger operator--(int);
  explicit operator bool() const;

  std::string toString() const;

 private:
  static const int BASE = 1'000'000'000;
  static const int SIZE = 9;

  enum class sign : int8_t {
    NEGATIVE = -1,
    ZERO = 0,
    POSITIVE = 1
  };

  std::vector<int> digits_;
  BigInteger::sign sign_;

  void remove_leading_zeroes();

  friend bool operator==(const BigInteger& first, const BigInteger& second);
  friend bool operator<(const BigInteger& first, const BigInteger& second);

  friend BigInteger find_quotient(BigInteger left, BigInteger right, const BigInteger& number, const BigInteger& divider);
};

// Declaration of operators

BigInteger operator+(const BigInteger& first, const BigInteger& second);
BigInteger operator-(const BigInteger& first, const BigInteger& second);
BigInteger operator*(const BigInteger& first, const BigInteger& second);
BigInteger operator/(const BigInteger& first, const BigInteger& second);
BigInteger operator%(const BigInteger& first, const BigInteger& second);

bool operator==(const BigInteger& first, const BigInteger& second);
bool operator<(const BigInteger& first, const BigInteger& second);
bool operator!=(const BigInteger& first, const BigInteger& second);
bool operator>=(const BigInteger& first, const BigInteger& second);
bool operator>(const BigInteger& first, const BigInteger& second);
bool operator<=(const BigInteger& first, const BigInteger& second);

std::ostream& operator<<(std::ostream& out, const BigInteger& big_integer);
std::istream& operator>>(std::istream& in, BigInteger& big_integer);

BigInteger operator""_bi(unsigned long long x);

// Additional functions

BigInteger abs(const BigInteger& bi) {
  if (bi < 0) {
    return -bi;
  }
  return bi;
}

void BigInteger::remove_leading_zeroes() {
  while (digits_.back() == 0 && digits_.size() > 1) {
    digits_.pop_back();
  }
}

BigInteger power(const BigInteger& value, int pow) {
  if (pow == 0) {
    return 1;
  }

  if (pow % 2 == 1) {
    return value * power(value, pow - 1);
  }

  return power((value * value), pow / 2);
}

BigInteger find_quotient(BigInteger left, BigInteger right, const BigInteger& number, const BigInteger& divider) {
  BigInteger sum;
  BigInteger middle;

  while (right - left > 1) {
    sum = right + left;
    middle = sum.digits_.front() / 2;

    for (size_t i = 1; i < sum.digits_.size(); ++i) {
      middle += 5 * power(10, BigInteger::SIZE * i - 1) * sum.digits_[i];
    }

    if (middle * divider == number) {
      return middle;
    }

    if (middle * divider > number) {
      right = middle;
      continue;
    }

    left = middle;
  }
  return left;
}

// Constructors

BigInteger::BigInteger(long long num) {
  sign_ = num > 0 ? (sign::POSITIVE) : (num == 0 ? sign::ZERO : sign::NEGATIVE);
  num *= static_cast<int>(sign_);

  if (num == 0) {
    digits_.push_back(0);
  } else {
    while (num > 0) {
      int digit = num % BASE;
      digits_.push_back(digit);
      num /= BASE;
    }
  }
}

BigInteger::BigInteger(const std::string& str) {
  if (str[0] == '-') {
    sign_ = sign::NEGATIVE;

    for (size_t i = str.size(); i > SIZE; i -= SIZE) {
      digits_.push_back(stoi(str.substr(i - SIZE, SIZE)));
    }

    if ((str.size() - 1) % SIZE != 0) {
      digits_.push_back(stoi(str.substr(1, str.size() - 1 - digits_.size() * SIZE)));
    }

  } else if (str[0] == '0') {
    sign_ = sign::ZERO;
    digits_.push_back(0);

  } else {
    sign_ = sign::POSITIVE;

    for (size_t i = str.size(); i >= SIZE; i -= SIZE) {
      digits_.push_back(stoi(str.substr(i - SIZE, SIZE)));
    }

    if (str.size() % SIZE != 0) {
      digits_.push_back(stoi(str.substr(0, str.size() - digits_.size() * SIZE)));
    }
  }
}

// Comparison operators

bool operator==(const BigInteger& first, const BigInteger& second) {
  if (first.sign_ != second.sign_) {
    return false;
  }

  if (first.sign_ == BigInteger::sign::ZERO) {
    return true;
  }

  return first.digits_ == second.digits_;
}

bool operator<(const BigInteger& first, const BigInteger& second) {
  if (first.sign_ != second.sign_) {
    return first.sign_ < second.sign_;
  }

  if (first.digits_.size() != second.digits_.size()) {
    return (first.digits_.size() < second.digits_.size()) == (first.sign_ == BigInteger::sign::POSITIVE);
  }

  for (size_t i = 0; i < first.digits_.size(); ++i) {
    if (first.digits_[first.digits_.size() - i - 1] != second.digits_[first.digits_.size() - i - 1]) {
      return first.digits_[first.digits_.size() - i - 1] < second.digits_[first.digits_.size() - i - 1];
    }
  }

  return false;
}

bool operator!=(const BigInteger& first, const BigInteger& second) {
  return !(first == second);
}

bool operator>=(const BigInteger& first, const BigInteger& second) {
  return !(first < second);
}

bool operator>(const BigInteger& first, const BigInteger& second) {
  return second < first;
}

bool operator<=(const BigInteger& first, const BigInteger& second) {
  return !(second < first);
}

// Calculation binary operators

BigInteger& BigInteger::operator+=(const BigInteger& other) {
  if (sign_ == other.sign_) {
    int carry = 0;
	size_t max_size = std::max(digits_.size(), other.digits_.size());

	for (size_t i = 0; i < max_size || carry > 0; ++i) {
	  if (i >= max_size && carry > 0) {
		digits_.push_back(carry);
		break;
	  }

	  long sum;
	  if (digits_.size() >= other.digits_.size()) {
		sum = (i < other.digits_.size()) ? (digits_[i] + other.digits_[i] + carry) : (digits_[i] + carry);
	  } else {
		sum = (i < digits_.size()) ? (digits_[i] + other.digits_[i] + carry) : (other.digits_[i] + carry);
	  }

	  carry = sum / BASE;
	  if (digits_.size() >= other.digits_.size() || i < digits_.size()) {
		digits_[i] = sum % BASE;
	  } else {
		digits_.push_back(sum % BASE);
	  }
	}

    remove_leading_zeroes();
    return *this;
  } else if (*this == 0) {

    *this = other;
    remove_leading_zeroes();
    return *this;
  }

  return *this -= -other;
}

BigInteger& BigInteger::operator-=(const BigInteger& other) {
  if (sign_ == other.sign_) {
    int carry = 0;
	size_t max_size = std::max(digits_.size(), other.digits_.size());
	if (abs(*this) == abs(other)) {
	  *this = 0;
	} else { // Change abs() to multiplication on sign
	  if (abs(*this) < abs(other)) {
		sign_ = (sign_ == BigInteger::sign::NEGATIVE) ? BigInteger::sign::POSITIVE : BigInteger::sign::NEGATIVE;
	  }
      for (size_t i = 0; i < max_size || carry != 0; ++i) {
		long diff;
		if (digits_.size() >= other.digits_.size()) {
		  diff = (i < other.digits_.size()) ? (digits_[i] - other.digits_[i] + carry) : (digits_[i] + carry);
		} else {
		  diff = (i < digits_.size() ? other.digits_[i] - digits_[i] + carry : other.digits_[i] + carry);
		}
        carry = 0;
        if (diff < 0) {
          carry = -1;
          diff += BASE;
        }
		if (digits_.size() >= other.digits_.size() || i < digits_.size()) {
		  digits_[i] = diff;
		} else {
		  digits_.push_back(diff);
		}
      }
    }
    remove_leading_zeroes();
    return *this;
  } else if (other == 0) {
    remove_leading_zeroes();
    return *this;
  }
  return *this += -other;
}

BigInteger& BigInteger::operator*=(const BigInteger& other) {
  if (abs(*this) > abs(other)) {
    std::vector<int> copy = digits_;
    int sign = static_cast<int>(sign_) * static_cast<int>(other.sign_);
    long long carry = 0;
    *this = 0;
    sign_ = sign::ZERO;
    for (size_t i = 0; i < other.digits_.size(); ++i) {
      BigInteger level = 0;
      level.digits_.clear();
      for (size_t j = 0; j < i; ++j) {
        level.digits_.push_back(0);
      }
      for (size_t j = 0; j < copy.size() || carry > 0; ++j) {
        if (j >= copy.size() && carry > 0) {
          level.digits_.push_back(carry);
          break;
        }
        long long number = static_cast<long long>(other.digits_[i]) * static_cast<long long>(copy[j]) + carry;
        carry = number / BASE;
        level.digits_.push_back(number % BASE);
        level.sign_ = sign::POSITIVE;
      }
      *this += level;
      carry = 0;
    }
    remove_leading_zeroes();
    sign_ = (sign == 0) ? (sign::ZERO) : (sign == 1 ? sign::POSITIVE : sign::NEGATIVE);
    return *this;
  }
  std::vector<int> copy = digits_;
  int sign = static_cast<int>(sign_) * static_cast<int>(other.sign_);
  long long carry = 0;
  *this = 0;
  sign_ = sign::ZERO;
  for (size_t i = 0; i < copy.size(); ++i) {
    BigInteger level = 0;
    level.digits_.clear();
    for (size_t j = 0; j < i; ++j) {
      level.digits_.push_back(0);
    }
    for (size_t j = 0; j < other.digits_.size() || carry > 0; ++j) {
      if (j >= other.digits_.size() && carry > 0) {
        level.digits_.push_back(carry);
        break;
      }
      long long number = static_cast<long long>(copy[i]) * static_cast<long long>(other.digits_[j]) + carry;
      carry = number / BASE;
      level.digits_.push_back(number % BASE);
      level.sign_ = sign::POSITIVE;
    }
    *this += level;
    carry = 0;
  }
  remove_leading_zeroes();
  sign_ = (sign == 0) ? (sign::ZERO) : (sign == 1 ? sign::POSITIVE : sign::NEGATIVE);
  return *this;
}

BigInteger& BigInteger::operator/=(const BigInteger& other) {
  int sign = static_cast<int>(sign_) * static_cast<int>(other.sign_);
  if (sign_ == sign::ZERO) {
    return *this;
  }
  if (abs(*this) < abs(other)) {
    *this = 0;
    remove_leading_zeroes();
    return *this;
  }
  if (abs(other) == 1) {
    sign_ = (sign == 0) ? (sign::ZERO) : (sign > 0 ? sign::POSITIVE : sign::NEGATIVE);
    return *this;
  }
  BigInteger quotient = 0;
  BigInteger digit;
  BigInteger dividend = digits_.back();
  for (int i = digits_.size() - 1; i >= 0;) {
    while (dividend < abs(other) && i >= 0) {
      --i;
      if (i < 0) {
        break;
      }
      quotient *= BASE;
      dividend *= BASE;
      dividend += digits_[i];
    }
    digit = find_quotient(0, BASE - 1, dividend, abs(other));
    quotient += digit;
    dividend -= abs(other) * digit;
    if (i <= 0) {
      break;
    }
  }
  *this = quotient;
  sign_ = (sign == 0) ? (sign::ZERO) : (sign > 0 ? sign::POSITIVE : sign::NEGATIVE);
  remove_leading_zeroes();
  return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& other) { return *this -= (*this / other) * other; }

BigInteger operator+(const BigInteger& first, const BigInteger& second) {
  BigInteger result = first;
  result += second;
  return result;
}

BigInteger operator-(const BigInteger& first, const BigInteger& second) {
  BigInteger result = first;
  result -= second;
  return result;
}

BigInteger operator*(const BigInteger& first, const BigInteger& second) {
  BigInteger result = first;
  result *= second;
  return result;
}

BigInteger operator/(const BigInteger& first, const BigInteger& second) {
  BigInteger result = first;
  result /= second;
  return result;
}

BigInteger operator%(const BigInteger& first, const BigInteger& second) {
  BigInteger result = first;
  result %= second;
  return result;
}

// Calculation unary operators

BigInteger BigInteger::operator-() const {
  BigInteger result = *this;
  result.sign_ = (sign_ == sign::ZERO) ? sign::ZERO : (sign_ == sign::NEGATIVE ? sign::POSITIVE : sign::NEGATIVE);
  return result;
}

BigInteger& BigInteger::operator++() { return (*this += 1); }

BigInteger BigInteger::operator++(int) {
  BigInteger copy(*this);
  *this += 1;
  return copy;
}

BigInteger& BigInteger::operator--() { return (*this -= 1); }

BigInteger BigInteger::operator--(int) {
  BigInteger copy(*this);
  *this -= 1;
  return copy;
}

// Operator bool() & literal suffix

BigInteger::operator bool() const {
  if (sign_ == sign::ZERO) {
    return false;
  }
  return true;
}

BigInteger operator""_bi(unsigned long long x) {
  BigInteger big_integer(x);
  return big_integer;
}

// toString() & input/output operators

std::string BigInteger::toString() const{
  std::string string;

  if (sign_ == sign::ZERO) {
    return "0";
  }

  if (sign_ == sign::NEGATIVE) {
    string.push_back('-');
  }

  string += std::to_string(digits_.back());

  for (int i = digits_.size() - 2; i >= 0; --i) {
    std::string digit = std::to_string(digits_[i]);
    std::string gap;

    for (size_t j = 0; j < SIZE - digit.size(); ++j) {
      gap.push_back('0');
    }

    string += gap;
    string += digit;
  }

  return string;
}

std::ostream& operator<<(std::ostream& out, const BigInteger& big_integer) {
  out << big_integer.toString();
  return out;
}

std::istream& operator>>(std::istream& in, BigInteger& big_integer) {
  std::string number;
  in >> number;
  big_integer = BigInteger(number);
  return in;
}
