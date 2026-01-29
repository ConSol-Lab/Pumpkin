from pathlib import Path


class Config:
    benchmark_dir = Path("./benchmarks/")
    pipeline_dir = Path("./pipelines/")
    experiments_dir = Path("./experiments/")
    sources_dir = Path("./sources/")
    results_dir = Path("./results/")
    figures_dir = Path("./figures/")

    @property
    def experiments_output_dir(self) -> Path:
        return self.experiments_dir / "outputs"

    @property
    def experiments_solvers_dir(self) -> Path:
        return self.experiments_dir / "solvers"

    @property
    def manifests_dir(self) -> Path:
        return self.experiments_dir / "manifests"
