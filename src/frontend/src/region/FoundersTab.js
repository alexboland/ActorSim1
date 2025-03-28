import React, { useState } from 'react';
import styles from './FoundersTab.module.css';

const FoundersTab = ({ founders }) => {
  const [selectedFounder, setSelectedFounder] = useState(null);
  const [expandedBonds, setExpandedBonds] = useState({});

  if (!founders || founders.length === 0) {
    return <div className={styles.emptyMessage}>No founders available for this region</div>;
  }

  // Helper function to safely check for site facility properties
  const hasFacility = (founder) => {
    return founder &&
           founder.site &&
           typeof founder.site === 'object' &&
           founder.site.facility;
  };

  const handleSelectFounder = (founder) => {
    setSelectedFounder(founder);
    // Reset expanded bonds when selecting a new founder
    setExpandedBonds({});
  };

  const toggleBondExpand = (bondId) => {
    setExpandedBonds(prev => ({
      ...prev,
      [bondId]: !prev[bondId]
    }));
  };

  const renderBonds = () => {
    if (!hasFacility(selectedFounder) ||
        !selectedFounder.site.facility.outstandingBonds ||
        Object.keys(selectedFounder.site.facility.outstandingBonds).length === 0) {
      return <div className={styles.emptyBonds}>No outstanding bonds</div>;
    }

    return (
      <div className={styles.bondsList}>
        {Object.entries(selectedFounder.site.facility.outstandingBonds).map(([bondId, bond]) => (
          <div key={bondId} className={styles.bondItem}>
            <div
              className={styles.bondHeader}
              onClick={() => toggleBondExpand(bondId)}
            >
              <div className={styles.bondId}>{bondId.slice(0, 8)}...</div>
              <div className={styles.bondAmount}>{bond.totalOutstanding}</div>
              <div className={styles.expandIcon}>
                {expandedBonds[bondId] ? '▼' : '►'}
              </div>
            </div>

            {expandedBonds[bondId] && (
              <div className={styles.bondDetails}>
                <div className={styles.bondDetailRow}>
                  <span className={styles.bondDetailLabel}>Principal:</span>
                  <span>{bond.principal}</span>
                </div>
                <div className={styles.bondDetailRow}>
                  <span className={styles.bondDetailLabel}>Interest Rate:</span>
                  <span>{(bond.interestRate * 100).toFixed(2)}%</span>
                </div>
                <div className={styles.bondDetailRow}>
                  <span className={styles.bondDetailLabel}>Outstanding:</span>
                  <span>{bond.totalOutstanding}</span>
                </div>
                <div className={styles.bondDetailRow}>
                  <span className={styles.bondDetailLabel}>Debtor ID:</span>
                  <span>{bond.debtorId.slice(0, 8)}...</span>
                </div>
              </div>
            )}
          </div>
        ))}
      </div>
    );
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
              {hasFacility(founder) ? 'Building' : 'Idle'}
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
              <div className={styles.infoLabel}>Status:</div>
              <div>{hasFacility(selectedFounder) ? 'Building' : 'Idle'}</div>
            </div>

            {hasFacility(selectedFounder) && (
              <>
                <div className={styles.infoRow}>
                  <div className={styles.infoLabel}>Resource:</div>
                  <div>{selectedFounder.site.facility.resourceProduced || 'N/A'}</div>
                </div>
                <div className={styles.infoRow}>
                  <div className={styles.infoLabel}>Percent complete:</div>
                  <div>{selectedFounder.site.percentComplete || 0}</div>
                </div>
                <div className={styles.infoRow}>
                  <div className={styles.infoLabel}>Workers:</div>
                  <div>{selectedFounder.site.facility.workers || 0} / {selectedFounder.site.facility.maxWorkers || 0}</div>
                </div>
                <div className={styles.infoRow}>
                  <div className={styles.infoLabel}>Wage:</div>
                  <div>{selectedFounder.site.facility.wage || 0}</div>
                </div>
                <div className={styles.infoRow}>
                  <div className={styles.infoLabel}>Stored Resources:</div>
                  <div>{Object.keys(selectedFounder.site.facility.storedResources || {}).length > 0 ?
                    Object.entries(selectedFounder.site.facility.storedResources).map(([key, value]) =>
                      `${key}: ${value}`).join(', ') : 'None'}</div>
                </div>
                <div className={styles.infoRow}>
                  <div className={styles.infoLabel}>Outstanding Bonds:</div>
                  <div>{selectedFounder.site.facility.outstandingBonds ?
                    Object.keys(selectedFounder.site.facility.outstandingBonds).length : 0}</div>
                </div>

                {renderBonds()}
              </>
            )}
          </div>
        </div>
      )}
    </div>
  );
};

export default FoundersTab;