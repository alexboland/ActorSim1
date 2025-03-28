import React from 'react';
import styles from './RegionModal.module.css';

const ResourcesTab = ({ storedResources }) => {
  if (!storedResources) {
    return <div>No resources available</div>;
  }

  return (
    <div>
      <h3 className={styles.sectionTitle}>Stored Resources</h3>
      <div className={styles.tableContainer}>
        <table className={styles.table}>
          <thead>
            <tr>
              <th className={styles.tableHeaderCell}>Resource</th>
              <th className={styles.tableHeaderCellRight}>Amount</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(storedResources).map(([resource, amount]) => (
              <tr key={resource} className={styles.tableRow}>
                <td className={styles.tableCell}>{resource}</td>
                <td className={styles.tableCellRight}>{amount}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
};

export default ResourcesTab;