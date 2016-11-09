#ifndef __CHECKINGACCOUNT_H_
#define __CHECKINGACCOUNT_H_

#include "Account.hpp"

class CheckingAccount : public Account {
    protected:
        double fee_per_transaction;
    public:
        CheckingAccount(double initial_deposit, double fee_per_transaction);
        void credit(double);
        bool debit(double);
};

#endif

