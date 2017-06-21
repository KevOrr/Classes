#pragma once

const double LOW_COMPETENCE = 25.0;
const double HI_COMPETENCE = 90.0;

class Subject
{
    friend double score(Subject* arr, int n);
    friend void swap(Subject& a, Subject& b);
protected:
    double competence;
    virtual double getBelief() const;
public:
    Subject();
    Subject(double comp);
    void setCompetence(double comp);
    double getCompetence() const;
    bool considersThemselfBetterThan(const Subject& oth) const;
    bool operator<(const Subject& oth) const;
    bool operator>(const Subject& oth) const;
};

class SlightlyOff : public Subject
{
private:
    virtual double getBelief() const;
};

class Delusional : public Subject
{
private:
    virtual double getBelief() const;
};

void swap(Subject& a, Subject& b);
void shuffle(Subject* arr, int n);
double score(Subject* arr, int n);
