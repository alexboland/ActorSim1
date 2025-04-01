import React, { useState, useEffect } from 'react';
import RegionsMap from './RegionsMap';
import ResourcesPanel from './ResourcesPanel';
import BankingPanel from './BankingPanel';
import styles from './GovernmentUI.module.css';

const GovernmentUI = ({ initialGovernment, initialRegions }) => {
  const [government, setGovernment] = useState(null);
  const [regions, setRegions] = useState([]);
  const [activeTab, setActiveTab] = useState('regions');
  const [selectedRegion, setSelectedRegion] = useState(null);
  const [isRefreshing, setIsRefreshing] = useState(false);

  // Effect to handle initial government setup
  useEffect(() => {
    if (initialGovernment) {
      setGovernment(initialGovernment);
    }
  }, [initialGovernment]);

  useEffect(() => {
    if (initialRegions) {
      setRegions(initialRegions);
    }
  }, [initialRegions]);

  const refreshGovernmentData = async () => {
    setIsRefreshing(true);
    try {
      const response = await fetch('http://localhost:8080/government/ping');
      if (response.ok) {
        const data = await response.json();
        setGovernment(data);
      } else {
        console.error('Failed to refresh government data:', response.statusText);
      }
    } catch (error) {
      console.error('Error refreshing government data:', error);
    } finally {
      setIsRefreshing(false);
    }
  };

  const updateRegionInfo = (uuid, info) => {
    setRegions(prevRegions => ({
      ...prevRegions,
      [uuid]: { ...prevRegions[uuid], ...info }
    }));
  };

  const handleViewDetails = (regionId) => {
    setSelectedRegion(regionId);
  };

  const handleCloseDetails = () => {
    setSelectedRegion(null);
  };

  if (!government) {
    return <div>Loading government data...</div>;
  }

  return (
    <div className={styles.governmentBox}>
      <h2 className={styles.heading}>Government Control Panel</h2>

      <div className={styles.tabContainer}>
        <div className={styles.tabsWrapper}>
          <div className={styles.tabs}>
            <button
              className={`${styles.tabButton} ${activeTab === 'regions' ? styles.activeTab : ''}`}
              onClick={() => setActiveTab('regions')}
            >
              Regions
            </button>
            <button
              className={`${styles.tabButton} ${activeTab === 'resources' ? styles.activeTab : ''}`}
              onClick={() => setActiveTab('resources')}
            >
              Resources
            </button>
            <button
              className={`${styles.tabButton} ${activeTab === 'banking' ? styles.activeTab : ''}`}
              onClick={() => setActiveTab('banking')}
            >
              Banking
            </button>
          </div>
          <button
            onClick={refreshGovernmentData}
            className={`${styles.refreshButton} ${isRefreshing ? styles.refreshing : ''}`}
            disabled={isRefreshing}
          >
            {isRefreshing ? 'Refreshing...' : 'Refresh Data'}
          </button>
        </div>

        <div className={styles.tabContent}>
          {activeTab === 'regions' && (
            <div>
              <RegionsMap
                regions={regions}
                connections={[]}
                onSelectRegion={handleViewDetails}
              />
            </div>
          )}

          {activeTab === 'resources' && (
            <ResourcesPanel
              government={government}
            />
          )}

          {activeTab === 'banking' && (
            <BankingPanel
              government={government}
            />
          )}
        </div>
      </div>
    </div>
  );
};

export default GovernmentUI;