#include <algorithm>
#include <cmath>
#include <compare>
#include <string>
#include <vector>
#include <iostream>
#include "biginteger.h"

// CLASS RATIONAL

class Rational {
 public:
  Rational() = default;
  Rational(int number);
  Rational(const BigInteger& number);
  Rational(const Rational& other) = default;
  ~Rational() = default;

  Rational& operator=(const Rational& other) = default;

  Rational& operator+=(const Rational& other);
  Rational& operator-=(const Rational& other);
  Rational& operator*=(const Rational& other);
  Rational& operator/=(const Rational& other);

  Rational operator-() const;
  explicit operator double() const;

  std::string toString() const;
  std::string asDecimal(size_t precision = 0) const;

 private:
  BigInteger numerator_;
  BigInteger denominator_;

  enum class sign : int8_t {
    NEGATIVE = -1,
    ZERO = 0,
    POSITIVE = 1
  };

  sign sign_;

  void reduction();

  friend bool operator==(const Rational& first, const Rational& second);
  friend bool operator<(const Rational& first, const Rational& second);
};

// Declaration of operators

Rational operator+(const Rational& first, const Rational& second);
Rational operator-(const Rational& first, const Rational& second);
Rational operator*(const Rational& first, const Rational& second);
Rational operator/(const Rational& first, const Rational& second);

bool operator!=(const Rational& first, const Rational& second);
bool operator>(const Rational& first, const Rational& second);
bool operator>=(const Rational& first, const Rational& second);
bool operator<=(const Rational& first, const Rational& second);

// Additional functions

BigInteger euclid(const BigInteger& first, const BigInteger& second) {
  if (second == 0) {
    return first;
  }

  if (second == 1) {
    return 1;
  }

  return euclid(second, first % second);
}

void Rational::reduction() {
  if (numerator_ == 0) {
    denominator_ = 1;
    return;
  }

  if (numerator_ % denominator_ == 0) {
    numerator_ /= denominator_;
    denominator_ = 1;
    return;
  }

  if (denominator_ % numerator_ == 0) {
    denominator_ /= numerator_;
    numerator_ = 1;
    return;
  }

  BigInteger nod;

  if (numerator_ < denominator_) {
    nod = euclid(denominator_, numerator_);
  } else {
    nod = euclid(numerator_, denominator_);
  }

  if (nod != 1) {
    denominator_ /= nod;
    numerator_ /= nod;
  }
}

Rational abs(const Rational& number) {
  if (number < 0) {
    return -number;
  }
  return number;
}

// Constructors

Rational::Rational(int number): numerator_(abs(number)), denominator_(1), sign_((number == 0) ? (sign::ZERO) : (number < 0 ? sign::NEGATIVE : sign::POSITIVE)) {}

Rational::Rational(const BigInteger& number): numerator_(abs(number)), denominator_(1), sign_((number == 0) ? (sign::ZERO) : (number < 0 ? sign::NEGATIVE : sign::POSITIVE)) {}

// Comparison operators

bool operator==(const Rational& first, const Rational& second) { return first.numerator_ == second.numerator_ && first.denominator_ == second.denominator_ && first.sign_ == second.sign_; }

bool operator<(const Rational& first, const Rational& second) {
  if (first.sign_ != second.sign_) {
    return first.sign_ < second.sign_;
  }

  if (first.sign_ == Rational::sign::ZERO) {
    return false;
  }

  if (first.sign_ == Rational::sign::POSITIVE) {
    return first.numerator_ * second.denominator_ < first.denominator_ * second.numerator_;
  }

  return first.numerator_ * second.denominator_ > first.denominator_ * second.numerator_;
}

bool operator!=(const Rational& first, const Rational& second) {
  return !operator==(first, second);
}

bool operator>(const Rational& first, const Rational& second) {
  return operator<(second, first);
}

bool operator>=(const Rational& first, const Rational& second) {
  return !operator<(first, second);
}

bool operator<=(const Rational& first, const Rational& second) {
  return !operator<(second, first);
}

// Calculation operators

Rational& Rational::operator+=(const Rational& other) {
  if (other.sign_ == sign::ZERO) {
    return *this;

  } else if (sign_ == other.sign_) {
    numerator_ = numerator_ * other.denominator_ + denominator_ * other.numerator_;
    denominator_ *= other.denominator_;

    reduction();
    return *this;
  }

  return *this -= -other;
}

Rational& Rational::operator-=(const Rational& other) {
  if (sign_ == sign::ZERO) {
    *this = -other;
    return *this;
  }

  if (sign_ == other.sign_) {
    if (abs(*this) > abs(other)) {
      numerator_ = numerator_ * other.denominator_ - denominator_ * other.numerator_;
      denominator_ *= other.denominator_;

      reduction();
      return *this;
    }

    if (abs(*this) == abs(other)) {
      *this = 0;
      return *this;
    }

    numerator_ = other.numerator_ * denominator_ - other.denominator_ * numerator_;
    denominator_ *= other.denominator_;
    sign_ = (sign_ == sign::POSITIVE) ? sign::NEGATIVE : sign::POSITIVE;

    reduction();
    return *this;
  }
  return *this += -other;
}

Rational& Rational::operator*=(const Rational& other) {
  int sign = static_cast<int>(sign_) * static_cast<int>(other.sign_);

  sign_ = (sign == 0) ? sign::ZERO : (sign > 0 ? sign::POSITIVE : sign::NEGATIVE);
  numerator_ *= other.numerator_;
  denominator_ *= other.denominator_;

  reduction();
  return *this;
}

Rational& Rational::operator/=(const Rational& other) {
  if (*this == 0) {
    return *this;
  }
  int sign = static_cast<int>(sign_) * static_cast<int>(other.sign_);

  sign_ = (sign == 0) ? sign::ZERO : (sign > 0 ? sign::POSITIVE : sign::NEGATIVE);
  numerator_ *= other.denominator_;
  denominator_ *= other.numerator_;

  reduction();
  return *this;
}

Rational operator+(const Rational& first, const Rational& second) {
  Rational result(first);
  result += second;
  return result;
}

Rational operator-(const Rational& first, const Rational& second) {
  Rational result(first);
  result -= second;
  return result;
}

Rational operator*(const Rational& first, const Rational& second) {
  Rational result(first);
  result *= second;
  return result;
}

Rational operator/(const Rational& first, const Rational& second) {
  Rational result(first);
  result /= second;
  return result;
}

Rational Rational::operator-() const {
  Rational result(*this);
  result.sign_ = (sign_ == sign::ZERO) ? (sign::ZERO) : (sign_ == sign::POSITIVE ? sign::NEGATIVE : sign::POSITIVE);
  return result;
}

// String methods

std::string Rational::toString() const {
  std::string number;

  if (sign_ == sign::ZERO) {
    return "0";
  }

  if (sign_ == sign::NEGATIVE) {
    number.push_back('-');
  }

  number += numerator_.toString();

  if (denominator_ != 1) {
    number.push_back('/');
    number += denominator_.toString();
  }

  return number;
}

std::string Rational::asDecimal(size_t precision) const {
  std::string string;

  if (sign_ == sign::ZERO) {
    return "0";
  }

  if (sign_ == sign::NEGATIVE) {
    string.push_back('-');
  }

  if (denominator_ == 1) {
    string += numerator_.toString();
    return string;
  }

  BigInteger pow = power(10, precision);
  BigInteger num = numerator_ * pow;
  num /= denominator_;
  string += (num / pow).toString();

  if (precision > 0) {
    string.push_back('.');
    BigInteger degree = 10;

    while (num * degree / pow == 0) {
      string += '0';
      degree *= 10;
    }

    string += (num % pow).toString();

    while (string.back() == '0') {
      string.pop_back();
    }
  }

  return string;
}

// Operator double()

Rational::operator double() const { return stod(asDecimal()); }
