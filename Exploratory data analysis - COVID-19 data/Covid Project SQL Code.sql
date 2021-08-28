Select *
From CovidProject.dbo.CovidDeaths
Where Continent is not null
Order by 3, 4


--Select data that will be used

Select Location, Date, total_cases, new_cases, total_deaths, population
From CovidProject.dbo.CovidDeaths
Order By 1, 2

-- Looking at Total Cases vs Total Deaths
-- Shows likelihood of dying if you were to contract covid in NZ
Select Location, Date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage
From CovidProject.dbo.CovidDeaths
Where location like 'new zealand'
And Continent is not null
Order By 1, 2

--Looking at Total Cases vs Population
-- Shows what percentage of population got Covid
Select Location, Date, total_cases, Population, (total_cases/population)*100 as PercentageCovid
From CovidProject.dbo.CovidDeaths
Where location like 'new zealand'
And Continent is not null
Order By 1, 2

-- Looking at Countries with Highest infection rate compared to population

Select Location, Population, MAX(total_cases) as HighestInfectionCount, MAX((Total_cases/population))*100 as PercentPopulationInfected
From CovidProject.dbo.CovidDeaths
Where Continent is not null
Group By Location, Population
Order By PercentPopulationInfected Desc

-- Showing the Countries with the Highest Death count per Population
Select Location, Max(cast(Total_deaths as int)) as TotalDeathCount
From CovidProject.dbo.CovidDeaths
Where Continent is not null
Group By Location
Order By TotalDeathCount Desc

--CONTINENTAL NUMBERS

-- Showing the Continents with the Highest Death count per Population
Select location, Max(cast(Total_deaths as int)) as TotalDeathCount
From CovidProject.dbo.CovidDeaths
Where Continent is null
Group By location
Order By TotalDeathCount Desc


--GLOBAL NUMBERS

--death percentage of new cases by date
Select Date, Sum(New_Cases) as Total_cases, Sum(cast(New_Deaths as int)) as Total_deaths, Sum(cast(New_Deaths as int))/Sum(New_cases)*100 as DeathPercentage
From CovidProject.dbo.CovidDeaths
Where Continent is not null
Group By date
Order By 1, 2

--fatality rate for total cases and total deaths
Select Sum(New_Cases) as Total_cases, Sum(cast(New_Deaths as int)) as Total_deaths, Sum(cast(New_Deaths as int))/Sum(New_cases)*100 as DeathPercentage
From CovidProject.dbo.CovidDeaths
Where Continent is not null
Order By 1, 2

-- Rolling vaccination count for each country
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as int)) over (partition by dea.location Order by dea.location, dea.date) as RollingVaccinationCount
From CovidProject.dbo.CovidDeaths as dea
Join Covidproject.dbo.CovidVaccinations as vac
	On dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null
order by 2, 3

--Rolling Vaccination Percentage
With PopvsVac (Continent, location, date, population, New_Vaccinations, RollingVaccinationCount)
as
(
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as int)) over (partition by dea.location Order by dea.location, dea.date) as RollingVaccinationCount
From CovidProject.dbo.CovidDeaths as dea
Join Covidproject.dbo.CovidVaccinations as vac
	On dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null
)
select *, (RollingVaccinationCount/population)*100 as RollingVaccinationPercentage
From PopvsVac
--where location like 'new zealand'

--Looking at which countries have the highest vaccination count per population
Drop Table if Exists #TotalVaccinations
Create Table #TotalVaccinations
(
Continent nvarchar(255),
Location nvarchar(255),
date datetime,
Population numeric,
New_Vaccinations numeric,
RollingVaccinationCount numeric
)

Insert into #TotalVaccinations
Select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations
, sum(cast(vac.new_vaccinations as int)) over (partition by dea.location Order by dea.location, dea.date) as RollingVaccinationCount
From CovidProject.dbo.CovidDeaths as dea
Join Covidproject.dbo.CovidVaccinations as vac
	On dea.location = vac.location
	and dea.date = vac.date
Where dea.continent is not null

select Location, Population, Max(RollingVaccinationCount) as TotalVaccinationCount, MAX((RollingVaccinationCount/Population))*100 as PercentagePopulationVaccinated
From #TotalVaccinations
Group By Location, Population
Order by 4 Desc

--Percentage of population fully vaccinated
Drop Table if Exists #TotalFullVaccinations
Create Table #TotalFullVaccinations
(
Continent nvarchar(255),
Location nvarchar(255),
date datetime,
Population numeric,
people_fully_vaccinated numeric,
RollingVaccinationCount numeric
)

Insert into #TotalFullVaccinations
Select dea.continent, dea.location, dea.date, dea.population, vac.people_fully_vaccinated
, sum(cast(vac.people_fully_vaccinated as bigint)) over (partition by dea.location Order by dea.location, dea.date) as RollingVaccinationCount
From CovidProject.dbo.CovidDeaths as dea
Join CovidProject.dbo.CovidVaccinations as vac
	ON dea.location = vac.location
	And dea.date = vac.date
Where dea.continent IS NOT Null

Select Location, Population, MAX(people_fully_vaccinated) as TotalFullyVaccinated, MAX((people_fully_vaccinated/Population)) as PercentageFullyVaccinated
From #TotalFullVaccinations
Group by Location, Population
Order by 4 desc
