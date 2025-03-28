import React, { useState } from 'react';
import styles from './BankTab.module.css';

const BankTab = ({ bank }) => {
  const [selectedBondType, setSelectedBondType] = useState('owned');

  if (!bank) {
    return <div>No bank data available</div>;
  }

  // Calculate total values
  const totalStoredMoney = bank.storedMoney || 0;
  const interestRate = bank.interestRate || 0;

  // Bond calculations
  const bondsOwned = bank.bondsOwned || {};
  const outstandingBonds = bank.outstandingBonds || {};

  const totalBondsOwnedCount = Object.keys(bondsOwned).length;
  const totalOutstandingBondsCount = Object.keys(outstandingBonds).length;

  const totalBondsOwnedValue = Object.values(bondsOwned).reduce(
    (sum, bond) => sum + (bond.totalOutstanding || 0),
    0
  );

  const totalOutstandingBondsValue = Object.values(outstandingBonds).reduce(
    (sum, bond) => sum + (bond.totalOutstanding || 0),
    0
  );

  const renderBondsList = (bonds) => {
    if (!bonds || Object.keys(bonds).length === 0) {
      return <div className={styles.emptyMessage}>No bonds in this category</div>;
    }

    return (
      <div className={styles.tableContainer}>
        <table className={styles.table}>
          <thead>
            <tr>
              <th className={styles.tableHeaderCell}>Bond ID</th>
              <th className={styles.tableHeaderCell}>Principal</th>
              <th className={styles.tableHeaderCell}>Interest Rate</th>
              <th className={styles.tableHeaderCellRight}>Outstanding</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(bonds).map(([id, bond]) => (
              <tr key={id} className={styles.tableRow}>
                <td className={styles.tableCell}>{id.slice(0, 8)}...</td>
                <td className={styles.tableCell}>{bond.principal}</td>
                <td className={styles.tableCell}>{(bond.interestRate * 100).toFixed(2)}%</td>
                <td className={styles.tableCellRight}>{bond.totalOutstanding}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    );
  };

  return (
    <div className={styles.bankTabContainer}>
      <div className={styles.summarySection}>
        <h3 className={styles.sectionTitle}>Bank Summary</h3>
        <div className={styles.summaryGrid}>
          <div className={styles.summaryCard}>
            <div className={styles.summaryLabel}>Stored Money</div>
            <div className={styles.summaryValue}>{totalStoredMoney}</div>
          </div>
          <div className={styles.summaryCard}>
            <div className={styles.summaryLabel}>Interest Rate</div>
            <div className={styles.summaryValue}>{(interestRate * 100).toFixed(2)}%</div>
          </div>
          <div className={styles.summaryCard}>
            <div className={styles.summaryLabel}>Bonds Owned</div>
            <div className={styles.summaryValue}>{totalBondsOwnedCount} (Value: {totalBondsOwnedValue})</div>
          </div>
          <div className={styles.summaryCard}>
            <div className={styles.summaryLabel}>Outstanding Bonds</div>
            <div className={styles.summaryValue}>{totalOutstandingBondsCount} (Value: {totalOutstandingBondsValue})</div>
          </div>
        </div>
      </div>

      <div className={styles.bondsSection}>
        <h3 className={styles.sectionTitle}>Bonds</h3>
        <div className={styles.bondsTabs}>
          <button
            className={`${styles.bondTabButton} ${selectedBondType === 'owned' ? styles.activeTab : ''}`}
            onClick={() => setSelectedBondType('owned')}
          >
            Bonds Owned ({totalBondsOwnedCount})
          </button>
          <button
            className={`${styles.bondTabButton} ${selectedBondType === 'outstanding' ? styles.activeTab : ''}`}
            onClick={() => setSelectedBondType('outstanding')}
          >
            Outstanding Bonds ({totalOutstandingBondsCount})
          </button>
        </div>

        <div className={styles.bondsListContainer}>
          {selectedBondType === 'owned' ? (
            renderBondsList(bondsOwned)
          ) : (
            renderBondsList(outstandingBonds)
          )}
        </div>
      </div>


    </div>
  );
};

export default BankTab;