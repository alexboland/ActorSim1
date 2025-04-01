import React, { useState } from 'react';
import styles from './BankingPanel.module.css';

const BankingPanel = ({ government }) => {
  const [interestRate, setInterestRate] = useState(
    government.interestRate || 0.05
  );

  const updateInterestRate = async () => {
    try {
      const response = await fetch('http://localhost:8080/government/setInterestRate', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          rate: interestRate
        })
      });

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      alert(`Interest rate updated successfully to ${(interestRate * 100).toFixed(2)}%`);
    } catch (error) {
      console.error('Failed to update interest rate:', error);
      alert(`Failed to update interest rate: ${error.message}`);
    }
  };

  return (
    <div className={styles.bankingPanel}>
      <h3 className={styles.sectionTitle}>Banking Controls</h3>

      <div className={styles.interestRateControl}>
        <h4>Set Central Bank Interest Rate</h4>
        <div className={styles.controlRow}>
          <div className={styles.inputWrapper}>
            <input
              type="number"
              min="0"
              max="1"
              step="0.01"
              value={interestRate}
              onChange={(e) => setInterestRate(Number(e.target.value))}
              className={styles.input}
            />
            <span className={styles.percentSign}>%</span>
          </div>
          <button
            onClick={updateInterestRate}
            className={styles.button}
          >
            Set Interest Rate
          </button>
        </div>
        <div className={styles.currentRate}>
          Current Base Interest Rate: {government.interestRate ? (government.interestRate * 100).toFixed(2) : '5.00'}%
        </div>
      </div>

      <div className={styles.bondsSection}>
        <h4>Available Bonds</h4>

        {government.bonds && Object.keys(government.bonds).length > 0 ? (
          <div className={styles.bondsTable}>
            <table className={styles.table}>
              <thead>
                <tr>
                  <th>ID</th>
                  <th>Principal</th>
                  <th>Interest Rate</th>
                  <th>Outstanding</th>
                  <th>Debtor</th>
                </tr>
              </thead>
              <tbody>
                {Object.entries(government.bonds).map(([id, bond]) => (
                  <tr key={id}>
                    <td>{id}</td>
                    <td>{bond.principal.toFixed(2)}</td>
                    <td>{(bond.interestRate * 100).toFixed(2)}%</td>
                    <td>{bond.totalOutstanding.toFixed(2)}</td>
                    <td>{bond.debtorId}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <div className={styles.noBonds}>
            No bonds are currently available.
          </div>
        )}
      </div>
    </div>
  );
};

export default BankingPanel;