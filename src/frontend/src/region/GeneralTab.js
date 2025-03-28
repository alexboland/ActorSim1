import React from 'react';
import modalStyles from './RegionModal.module.css';
import styles from './GeneralTab.module.css';

const GeneralTab = ({ region }) => {
console.log("======")
console.log(region)
  return (
    <div>
      <h3 className={modalStyles.sectionTitle}>Region Information</h3>
      <div className={modalStyles.infoSection}>
        <div className={modalStyles.infoRow}>
          <span className={modalStyles.infoLabel}>ID:</span>
          <span>{region.id}</span>
        </div>
        <div className={modalStyles.infoRow}>
          <span className={modalStyles.infoLabel}>Population:</span>
          <span>{region.population}</span>
        </div>
        <div className={modalStyles.infoRow}>
          <span className={modalStyles.infoLabel}>Location:</span>
          <span>X: {region.location?.x}, Y: {region.location?.y}</span>
        </div>
        <div className={modalStyles.infoRow}>
          <span className={modalStyles.infoLabel}>Season:</span>
          <span>{region.season}</span>
        </div>
      </div>

      <h3 className={modalStyles.sectionTitle}>Stored Resources</h3>
      <div className={styles.resourceGrid}>
        {Object.entries(region.storedResources).map(([resource, amount]) => (
          <div key={resource} className={styles.resourceCard}>
            <div className={styles.resourceName}>{resource}</div>
            <div className={styles.resourceAmount}>{amount.toLocaleString()}</div>
          </div>
        ))}
      </div>

      <h3 className={modalStyles.sectionTitle}>Base Production</h3>
      <div className={modalStyles.tableContainer}>
        <table className={modalStyles.table}>
          <thead>
            <tr>
              <th className={modalStyles.tableHeaderCell}>Resource</th>
              <th className={modalStyles.tableHeaderCellRight}>Amount per Day</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(region.baseProduction).map(([resource, amount]) => (
              <tr key={resource} className={modalStyles.tableRow}>
                <td className={modalStyles.tableCell}>{resource}</td>
                <td className={modalStyles.tableCellRight}>{amount.toFixed(2)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default GeneralTab;