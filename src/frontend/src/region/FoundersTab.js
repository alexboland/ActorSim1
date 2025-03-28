import React, { useState } from 'react';
import styles from './FoundersTab.module.css';

const FoundersTab = ({ founders }) => {
  const [selectedFounder, setSelectedFounder] = useState(null);

  if (!founders || founders.length === 0) {
    return <div className={styles.emptyMessage}>No founders available for this region</div>;
  }

  const handleSelectFounder = (founder) => {
    setSelectedFounder(founder);
  };

  return (
    <div className={styles.foundersTabContainer}>
      <div className={styles.foundersHeader}>
        <h3 className={styles.sectionTitle}>Region Founders ({founders.length})</h3>
      </div>

      <div className={styles.foundersGrid}>
        {founders.map((founder) => (
          <div
            key={founder.id}
            className={`${styles.founderCard} ${selectedFounder && selectedFounder.id === founder.id ? styles.selectedFounder : ''}`}
            onClick={() => handleSelectFounder(founder)}
          >
            <div className={styles.founderTitle}>Founder</div>
            <div className={styles.founderId}>{founder.id.slice(0, 8)}...</div>
            <div className={styles.founderSite}>
              Site: {founder.site ? founder.site : 'None'}
            </div>
          </div>
        ))}
      </div>

      {selectedFounder && (
        <div className={styles.founderDetails}>
          <h4 className={styles.founderDetailsTitle}>Founder Details</h4>
          <div className={styles.infoSection}>
            <div className={styles.infoRow}>
              <div className={styles.infoLabel}>ID:</div>
              <div>{selectedFounder.id}</div>
            </div>
            <div className={styles.infoRow}>
              <div className={styles.infoLabel}>Region ID:</div>
              <div>{selectedFounder.regionId}</div>
            </div>
            <div className={styles.infoRow}>
              <div className={styles.infoLabel}>Site:</div>
              <div>{selectedFounder.site ? selectedFounder.site : 'None'}</div>
            </div>
          </div>

          <div className={styles.jsonSection}>
            <h5 className={styles.jsonTitle}>Raw Data</h5>
            <pre className={styles.jsonDisplay}>
              {JSON.stringify(selectedFounder, null, 2)}
            </pre>
          </div>
        </div>
      )}
    </div>
  );
};

export default FoundersTab;