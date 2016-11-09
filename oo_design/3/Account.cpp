#include <iostream>

#include "Account.hpp"

using namespace std;

Account::Account(double initial_deposit) {
    if (initial_deposit < 0) {
        cerr << "Account() called with a negative balance, assuming 0.00 instead" << endl;
        balance = 0;
    } else {
        balance = initial_deposit;
    }
}

void Account::credit(double ammount) {
    balance += ammount;
}

bool Account::debit(double ammount) {
    if (ammount > balance) {
        cerr << "Debit ampunt exceeded account balance." << endl;
        return false;
    }
    balance -= ammount;
    return true;
}

double Account::getBalance() {
    return balance;
}

