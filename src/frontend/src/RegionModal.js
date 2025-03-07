// Add this to the beginning of your RegionModal.js file to test if the modal is rendering

import React, { useState, useEffect } from 'react';

// Main Region Modal Component
const RegionModal = ({ isOpen, onClose, regionId }) => {
  const [activeTab, setActiveTab] = useState('general');
  const [regionData, setRegionData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [selectedProducer, setSelectedProducer] = useState(null);

  // Debug flag - set to true to use basic styles without Tailwind
  const DEBUG_MODE = false;

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

  // Other functions stay the same...

  if (!isOpen) {
    console.log("Modal is closed, returning null");
    return null;
  }

  // For debugging - use a simple div with inline styles
  if (DEBUG_MODE) {
    return (
      <div style={{
        position: 'fixed',
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        backgroundColor: 'rgba(0,0,0,0.75)',
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        zIndex: 9999
      }}>
        <div style={{
          backgroundColor: 'white',
          borderRadius: '8px',
          padding: '20px',
          width: '80%',
          maxWidth: '800px',
          maxHeight: '80vh',
          overflow: 'auto',
          boxShadow: '0 4px 8px rgba(0,0,0,0.2)'
        }}>
          <div style={{
            display: 'flex',
            justifyContent: 'space-between',
            marginBottom: '20px'
          }}>
            <h2 style={{ fontSize: '1.5rem', fontWeight: 'bold' }}>
              {loading ? 'Loading Region...' : regionData ? `Region: ${regionData.region.id}` : 'Region Details'}
            </h2>
            <button 
              onClick={onClose}
              style={{
                background: 'none',
                border: 'none',
                fontSize: '1.5rem',
                cursor: 'pointer'
              }}
            >
              ×
            </button>
          </div>

          {loading ? (
            <div>Loading data...</div>
          ) : error ? (
            <div style={{ color: 'red' }}>{error}</div>
          ) : regionData ? (
            <div>
              <pre>{JSON.stringify(regionData, null, 2)}</pre>
            </div>
          ) : (
            <div>No data available</div>
          )}

          <div style={{ marginTop: '20px', textAlign: 'right' }}>
            <button
              onClick={fetchRegionData}
              style={{
                backgroundColor: '#3b82f6',
                color: 'white',
                padding: '8px 16px',
                borderRadius: '4px',
                border: 'none',
                cursor: 'pointer'
              }}
            >
              Refresh
            </button>
          </div>
        </div>
      </div>
    );
  }

  // Original return with Tailwind styles
  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      {/* Rest of your original modal code */}
    </div>
  );
};

// Rest of your component code...

export default RegionModal;