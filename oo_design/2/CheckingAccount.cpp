#include <iostream>

#include "CheckingAccount.hpp"
#include "Account.hpp"

using namespace std;

CheckingAccount::CheckingAccount(double initial_deposit, double fee_per_transaction)
    : Account(initial_deposit),
      fee_per_transaction(fee_per_transaction)
    {}

void CheckingAccount::credit(double ammount) {
    Account::credit(ammount);
    Account::debit(fee_per_transaction);
}

bool CheckingAccount::debit(double ammount) {
    if (Account::debit(ammount)) {
        return Account::debit(fee_per_transaction);
    } else {
        return false;
    }
}

