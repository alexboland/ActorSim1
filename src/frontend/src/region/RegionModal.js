import React, { useState, useEffect } from 'react';
import styles from './RegionModal.module.css';
import GeneralTab from './GeneralTab';
import ResourcesTab from './ResourcesTab';
import ProducersTab from './ProducersTab';
import MarketTab from './MarketTab';
import BankTab from './BankTab';
import FoundersTab from './FoundersTab';

// Main Region Modal Component
const RegionModal = ({ isOpen, onClose, regionId }) => {
  const [activeTab, setActiveTab] = useState('general');
  const [regionData, setRegionData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    if (isOpen && regionId) {
      console.log("Modal should be visible now");
      fetchRegionData();
    }
  }, [isOpen, regionId]);

  const fetchRegionData = async () => {
    setLoading(true);
    try {
      // Log the URL being fetched for debugging
      const url = `http://localhost:8080/regions/${regionId}/fullDetails`;
      console.log("Fetching region data from:", url);

      const response = await fetch(url);

      // Log the response status and content type
      console.log("Response status:", response.status);
      console.log("Response content type:", response.headers.get("content-type"));

      if (!response.ok) {
        throw new Error(`Failed to fetch region data: ${response.status} ${response.statusText}`);
      }

      // For debugging, get the raw text first
      const responseText = await response.text();

      // Try to parse it as JSON, but if it fails, log the raw text
      try {
        const data = JSON.parse(responseText);
        console.log("Parsed data successfully:", data);
        setRegionData(data);
        setError(null);
      } catch (parseError) {
        console.error("Error parsing JSON response:", parseError);
        console.error("Raw response:", responseText.substring(0, 500) + "..."); // Log the first 500 chars
        throw new Error(`Invalid JSON response: ${parseError.message}`);
      }
    } catch (err) {
      setError(err.message);
      console.error("Error fetching region data:", err);
    } finally {
      setLoading(false);
    }
  };

  if (!isOpen) {
    console.log("Modal is closed, returning null");
    return null;
  }

  return (
    <div className={styles.modalOverlay}>
      <div className={styles.modalContainer}>
        <div className={styles.modalHeader}>
          <h2 className={styles.modalTitle}>
            {loading ? 'Loading Region...' : regionData ? `Region: ${regionData.region.id.slice(0, 8)}` : 'Region Details'}
          </h2>
          <button
            onClick={onClose}
            className={styles.closeButton}
          >
            ×
          </button>
        </div>

        {loading ? (
          <div>Loading data...</div>
        ) : error ? (
          <div className={styles.errorMessage}>{error}</div>
        ) : regionData ? (
          <div>
            <div className={styles.tabsContainer}>
              <button
                onClick={() => setActiveTab('general')}
                className={`${styles.tabButton} ${activeTab === 'general' ? styles.activeTab : ''}`}
              >
                General
              </button>
              <button
                onClick={() => setActiveTab('producers')}
                className={`${styles.tabButton} ${activeTab === 'producers' ? styles.activeTab : ''}`}
              >
                Producers
              </button>
              <button
                onClick={() => setActiveTab('founders')}
                className={`${styles.tabButton} ${activeTab === 'founders' ? styles.activeTab : ''}`}
              >
                Founders
              </button>
              <button
                onClick={() => setActiveTab('market')}
                className={`${styles.tabButton} ${activeTab === 'market' ? styles.activeTab : ''}`}
              >
                Market
              </button>
              <button
                onClick={() => setActiveTab('bank')}
                className={`${styles.tabButton} ${activeTab === 'bank' ? styles.activeTab : ''}`}
              >
                Bank
              </button>
            </div>

            {activeTab === 'general' && <GeneralTab region={regionData.region} />}
            {activeTab === 'producers' && <ProducersTab producers={regionData.producers} />}
            {activeTab === 'founders' && <FoundersTab founders={regionData.founders} />}
            {activeTab === 'market' && <MarketTab market={regionData.market} />}
            {activeTab === 'bank' && <BankTab bank={regionData.bank} />}
          </div>
        ) : (
          <div>No data available</div>
        )}

        <div className={styles.modalFooter}>
          <button
            onClick={fetchRegionData}
            className={styles.primaryButton}
          >
            Refresh
          </button>
          <button
            onClick={onClose}
            className={styles.secondaryButton}
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
};

export default RegionModal;