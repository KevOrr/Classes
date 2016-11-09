#include <iostream>

#include "SavingsAccount.hpp"
#include "Account.hpp"

using namespace std;

SavingsAccount::SavingsAccount(double initial_deposit, double interest)
    : Account(initial_deposit),
      interest(interest)
    {}

double SavingsAccount::getInterest() {
    return interest;
}

double SavingsAccount::calculateInterest() {
    return interest * balance;
}

