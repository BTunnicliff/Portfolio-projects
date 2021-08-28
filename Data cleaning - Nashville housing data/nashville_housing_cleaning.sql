--Cleaning data in SQL queries

Select *
From DataCleaningProject.dbo.NashvilleHousing

--The SaleDate column is formatted as datetime, however the time is always set to 00:00, so I will set it to be just the date.
Select SaleDate, CONVERT(Date, SaleDate)
From DataCleaningProject.dbo.NashvilleHousing

Alter Table NashvilleHousing
Add SaleDateConverted Date;

Update NashvilleHousing
SET SaleDateConverted = CONVERT(Date, SaleDate)

--Some property addresses are missing, however they can be populated
Select *
From DataCleaningProject.dbo.NashvilleHousing
--Where PropertyAddress IS NULL
Order By ParcelID

--Missing addresses can be found by matching parcelIDs between the missing and non-missing addresses

Select a.ParcelID, a.PropertyAddress, b.ParcelID, b.PropertyAddress, ISNULL(a.PropertyAddress, b.PropertyAddress)
From DataCleaningProject.dbo.NashvilleHousing a
JOIN DataCleaningProject.dbo.NashvilleHousing b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress IS NULL
--Joining the dataset to itself allows you to find situations in which a null address with ID x in table a has an equal ID with a non missing address in table b which can replace the missing address.
Update a
Set PropertyAddress = ISNULL(a.PropertyAddress, b.PropertyAddress)
From DataCleaningProject.dbo.NashvilleHousing a
JOIN DataCleaningProject.dbo.NashvilleHousing b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress IS NULL

--Address should be separated into individual columns for Address, City, and State.

Select
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1) as Address
, Substring(PropertyAddress, CHARINDEX(',', PropertyAddress) + 1, LEN(PropertyAddress)) as City
From DataCleaningProject.dbo.NashvilleHousing

Alter Table NashvilleHousing
Add PropertySplitAddress Nvarchar(255), PropertySplitCity Nvarchar(255);

Update NashvilleHousing
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1)
	, PropertySplitCity = Substring(PropertyAddress, CHARINDEX(',', PropertyAddress) + 1, LEN(PropertyAddress))

--Now to split the owner address
Select OwnerAddress
From DataCleaningProject.dbo.NashvilleHousing

--The address, city, and state are separated by commas.
Select
PARSENAME(Replace(OwnerAddress, ',', '.'), 3) as address
, PARSENAME(Replace(OwnerAddress, ',', '.'), 2) as city
, PARSENAME(Replace(OwnerAddress, ',', '.'), 1) as state
From DataCleaningProject.dbo.NashvilleHousing

Alter Table NashvilleHousing
Add OwnerSplitAddress Nvarchar(255), OwnerSplitCity Nvarchar(255), OwnerSplitState Nvarchar(255);

Update NashvilleHousing
Set OwnerSplitAddress = PARSENAME(Replace(OwnerAddress, ',', '.'), 3)
	, OwnerSplitCity = PARSENAME(Replace(OwnerAddress, ',', '.'), 2)
	, OwnerSplitState = PARSENAME(Replace(OwnerAddress, ',', '.'), 1)

-- Removing double spaces from the split address columns.
Select PropertySplitAddress, REPLACE(PropertySplitAddress, '  ', ' ')
From DataCleaningProject.dbo.NashvilleHousing

Update NashvilleHousing
set PropertySplitAddress = REPLACE(PropertySplitAddress, '  ', ' ')

Select OwnerSplitAddress, REPLACE(OwnerSplitAddress, '  ', ' ')
From DataCleaningProject.dbo.NashvilleHousing

Update NashvilleHousing
set OwnerSplitAddress = REPLACE(OwnerSplitAddress, '  ', ' ')

--Trimming whitespace from split city and state columns
Select PropertySplitCity, TRIM(PropertySplitCity)
From DataCleaningProject.dbo.NashvilleHousing

UPDATE NashvilleHousing
Set PropertySplitCity = TRIM(PropertySplitCity), OwnerSplitCity = TRIM(OwnerSplitCity), OwnerSplitState = TRIM(OwnerSplitState)

--In the Sold as Vacant column there are values Y and N which should be changed to Yes and No
Select Distinct(SoldAsVacant), COUNT(SoldAsVacant)
From DataCleaningProject.dbo.NashvilleHousing
Group By SoldAsVacant
Order by 2

Select SoldAsVacant
, Case When SoldAsVacant = 'Y' THEN 'Yes'
		When SoldAsVacant = 'N' THEN 'No'
		ELSE SoldAsVacant
		END
From DataCleaningProject.dbo.NashvilleHousing

Update NashvilleHousing
Set SoldAsVacant = Case When SoldAsVacant = 'Y' THEN 'Yes'
						When SoldAsVacant = 'N' THEN 'No'
						ELSE SoldAsVacant
						END

--Delete Unused Columns
Alter Table DataCleaningProject.dbo.NashvilleHousing
Drop Column OwnerAddress, TaxDistrict, PropertyAddress, Saledate

Select *
From DataCleaningProject.dbo.NashvilleHousing