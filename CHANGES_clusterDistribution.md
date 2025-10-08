# clusterDistribution Function Update

## Date: 24 September 2025

## Summary of Changes

The `clusterDistribution()` function has been updated to support multiple features and fix the total calculation bug.

## Issues Fixed

### 1. **Total Calculation Bug**
- **Problem**: The function was using an undefined variable `result_df` instead of `cont_table` when calculating totals
- **Fix**: Properly initialize `result_df` from `cont_table` and use correct variable names throughout

### 2. **Single Feature Limitation**
- **Problem**: Function only accepted a single feature string
- **Fix**: Now accepts either a single feature or a vector of features for multi-dimensional analysis

### 3. **Missing Values Handling**
- **Problem**: Missing values calculation only worked for single feature
- **Fix**: Now calculates missing values for each feature separately and returns a named vector

## New Features

### Multiple Feature Support
```r
# Single feature (original functionality)
dist1 <- clusterDistribution(clustered_data, "treatment")

# Multiple features (NEW)
dist2 <- clusterDistribution(clustered_data, c("treatment", "age_group"))

# Output structure for multiple features:
#   treatment  age_group  cluster1  cluster2  cluster3  Total
#   Control    Young      10        5         3         18
#   Control    Old        15        10        7         32
#   Treatment  Young      8         15        10        33
#   Treatment  Old        12        15        15        42
#   TOTAL      TOTAL      45        45        35        125
```

### Enhanced Metadata
The function now returns additional metadata attributes:
- `feature`: Vector of feature names analyzed
- `n_clusters`: Number of clusters
- `total_subjects`: Total number of subjects (correctly calculated)
- `missing_values`: Named vector with missing count per feature
- `n_complete_cases`: Number of subjects with complete data for all requested features

## Implementation Details

### Key Changes in Code

1. **Feature Validation Loop**: Now validates all features in the input vector
2. **Complete Cases**: Uses `complete.cases()` to handle multiple feature missing values
3. **Dynamic Column Identification**: Identifies cluster columns dynamically (all non-feature columns)
4. **Correct Total Calculation**: 
   - Total row: Sums across all cluster columns
   - Total column: Sums across all cluster columns for each row
5. **Column Naming**: Consistent "cluster" prefix for all cluster columns

### Function Signature
```r
clusterDistribution(
  object,              # CONNECTORDataClustered object
  feature,             # Single feature or vector of features
  include_totals = TRUE  # Include total row and column
)
```

## Testing

A test script has been created at `test/test_clusterDistribution.R` that demonstrates:
- Single feature usage
- Multiple feature usage
- With/without totals
- Metadata access

## Documentation Updates

### Updated Files
1. **R/CONNECTORDataClustered-class.R**: Function implementation
2. **README.md**: Usage examples and documentation
3. **test/test_clusterDistribution.R**: Test script

### Documentation Sections Updated
- Key Functions table
- Cluster Composition Analysis section with multi-feature examples
- Metadata attributes documentation

## Usage Examples

### Basic Usage
```r
# Single feature
result <- clusterDistribution(clustered_data, "treatment")
print(result)

# Multiple features
result <- clusterDistribution(clustered_data, c("treatment", "age_group"))
print(result)

# Access metadata
total_n <- attr(result, "total_subjects")
missing <- attr(result, "missing_values")
complete <- attr(result, "n_complete_cases")
```

### Integration with Analysis Workflow
```r
# After clustering
clustered_data <- selectCluster(clusters, G=3, "MinfDB")

# Analyze single features
treatment_dist <- clusterDistribution(clustered_data, "treatment")
age_dist <- clusterDistribution(clustered_data, "age")

# Analyze feature combinations
combined_dist <- clusterDistribution(clustered_data, c("treatment", "age", "gender"))

# Check for cluster-feature associations
print(treatment_dist)
```

## Backwards Compatibility

The function remains fully backwards compatible:
- Single feature usage works exactly as before
- Default parameter values unchanged
- Return structure consistent with previous version for single features
- Only enhancement is support for multiple features

## Performance Considerations

- Multiple features increase the number of unique combinations
- Memory usage scales with feature cardinality product
- For high-cardinality features, consider analyzing separately
- Complete cases filtering may reduce sample size significantly with many features

## Future Enhancements

Potential future improvements:
1. Add percentage calculation option (was in signature but not implemented)
2. Support for grouped/stratified analysis
3. Statistical tests for cluster-feature associations
4. Visualization functions for distribution tables
5. Export to various formats (CSV, Excel, etc.)
