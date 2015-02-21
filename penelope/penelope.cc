// This is free and unencumbered software released into the public domain.
//
// Anyone is free to copy, modify, publish, use, compile, sell, or
// distribute this software, either in source code form or as a compiled
// binary, for any purpose, commercial or non-commercial, and by any
// means.

#include "kernel/satgen.h"
#include "penelope/core/Cooperation.h"
#include "penelope/core/Solver.h"

using namespace penelope;
USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

const int nbThreads = 1;
const int limitExport = 10;

struct ezPenelope : public ezSAT
{
private:
	INIParser parser;
	Cooperation* coop;
	std::vector<int> satVars;
	bool foundContradiction;

public:
	ezPenelope() : parser("penelope_config.ini")
	{
		coop = nullptr;
		foundContradiction = false;
		parser.parse();
	}

	virtual ~ezPenelope()
	{
		if (coop)
			delete coop;
	}

	virtual void clear()
	{
		if (coop)
			delete coop;
		coop = nullptr;
		foundContradiction = false;
	}

	virtual bool solver(const std::vector<int> &modelExpressions, std::vector<bool> &modelValues, const std::vector<int> &assumptions)
	{
		preSolverCallback();

		if (0) {
contradiction:
			if (coop)
				delete coop;
			coop = nullptr;
			foundContradiction = true;
			return false;
		}
	
		if (foundContradiction) {
			consumeCnf();
			return false;
		}

		std::vector < int >extraClauses, modelIdx;

		for (auto id:assumptions)
			extraClauses.push_back(bind(id));
		for (auto id:modelExpressions)
			modelIdx.push_back(bind(id));

		if (coop == nullptr) {
			log_assert(nbThreads == 1);
			coop = new Cooperation(nbThreads, limitExport);
			for (int i = 0; i < nbThreads; i++) {
				coop->solvers[0].initialize(coop, i, parser);
				coop->solvers[0].verbosity = 0;
				coop->solvers[0].deterministic_mode = 0;
			}
		}

		std::vector<std::vector<int>> cnf;
		consumeCnf(cnf);

		while (int(satVars.size()) < numCnfVariables())
			satVars.push_back(coop->solvers[0].newVar());

		for (auto &clause : cnf) {
			penelope::vec<Lit> ps;
			for (auto idx : clause) {
				if (idx > 0)
					ps.push(mkLit(satVars.at(idx - 1)));
				else
					ps.push(mkLit(satVars.at(-idx - 1), true));
			}
			if (!coop->solvers[0].addClause(ps))
				goto contradiction;
		}

		if (cnf.size() > 0 && !coop->solvers[0].simplify(coop))
			goto contradiction;

		penelope::vec<Lit> assumps;

		for (auto idx : extraClauses) {
			if (idx > 0)
				assumps.push(mkLit(satVars.at(idx-1)));
			else
				assumps.push(mkLit(satVars.at(-idx-1), true));
		}

		bool foundSolution = coop->solvers[0].solve(assumps, coop);

		if (!foundSolution)
			return false;

		modelValues.clear();
		modelValues.resize(modelIdx.size());

		for (size_t i = 0; i < modelIdx.size(); i++) {
			int idx = modelIdx[i];
			bool refvalue = true;

			if (idx < 0)
				idx = -idx, refvalue = false;

			lbool value = coop->solvers[0].modelValue(satVars.at(idx - 1));
			modelValues[i] = (value == lbool(refvalue));
		}

		return true;
	}
};

struct PenelopeSatSolver : public SatSolver {
	PenelopeSatSolver() : SatSolver("penelope") {
		yosys_satsolver = this;
	}
	virtual ezSAT *create() YS_OVERRIDE {
		return new ezPenelope();
	}
} PenelopeSatSolver;

PRIVATE_NAMESPACE_END
