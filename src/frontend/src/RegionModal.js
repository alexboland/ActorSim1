import React, { useState, useEffect } from 'react';

// Main Region Modal Component
const RegionModal = ({ isOpen, onClose, regionId }) => {
  const [activeTab, setActiveTab] = useState('general');
  const [regionData, setRegionData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [selectedProducer, setSelectedProducer] = useState(null);

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

  // Using inline styles instead of Tailwind to ensure consistent rendering
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
            {loading ? 'Loading Region...' : regionData ? `Region: ${regionData.region.id.slice(0, 8)}` : 'Region Details'}
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
            <div style={{
              display: 'flex',
              borderBottom: '1px solid #e2e8f0',
              marginBottom: '16px'
            }}>
              <button
                onClick={() => setActiveTab('general')}
                style={{
                  padding: '8px 16px',
                  borderBottom: activeTab === 'general' ? '2px solid #3b82f6' : 'none',
                  backgroundColor: activeTab === 'general' ? '#eff6ff' : 'transparent',
                  fontWeight: activeTab === 'general' ? 'bold' : 'normal',
                  marginRight: '8px'
                }}
              >
                General
              </button>
              <button
                onClick={() => setActiveTab('resources')}
                style={{
                  padding: '8px 16px',
                  borderBottom: activeTab === 'resources' ? '2px solid #3b82f6' : 'none',
                  backgroundColor: activeTab === 'resources' ? '#eff6ff' : 'transparent',
                  fontWeight: activeTab === 'resources' ? 'bold' : 'normal',
                  marginRight: '8px'
                }}
              >
                Resources
              </button>
              <button
                onClick={() => setActiveTab('producers')}
                style={{
                  padding: '8px 16px',
                  borderBottom: activeTab === 'producers' ? '2px solid #3b82f6' : 'none',
                  backgroundColor: activeTab === 'producers' ? '#eff6ff' : 'transparent',
                  fontWeight: activeTab === 'producers' ? 'bold' : 'normal'
                }}
              >
                Producers
              </button>
            </div>

            {activeTab === 'general' && (
              <div>
                <h3 style={{ fontSize: '1.25rem', fontWeight: 'bold', marginBottom: '8px' }}>Region Information</h3>
                <div style={{ marginBottom: '16px' }}>
                  <div style={{ display: 'flex', marginBottom: '4px' }}>
                    <span style={{ fontWeight: 'bold', width: '150px' }}>ID:</span>
                    <span>{regionData.region.id}</span>
                  </div>
                  <div style={{ display: 'flex', marginBottom: '4px' }}>
                    <span style={{ fontWeight: 'bold', width: '150px' }}>Population:</span>
                    <span>{regionData.region.population}</span>
                  </div>
                  <div style={{ display: 'flex', marginBottom: '4px' }}>
                    <span style={{ fontWeight: 'bold', width: '150px' }}>Location:</span>
                    <span>X: {regionData.region.location?.x}, Y: {regionData.region.location?.y}</span>
                  </div>
                </div>
              </div>
            )}

            {activeTab === 'resources' && regionData.storedResources && (
              <div>
                <h3 style={{ fontSize: '1.25rem', fontWeight: 'bold', marginBottom: '8px' }}>Stored Resources</h3>
                <div style={{
                  border: '1px solid #e2e8f0',
                  borderRadius: '4px',
                  overflow: 'hidden'
                }}>
                  <table style={{ width: '100%', borderCollapse: 'collapse' }}>
                    <thead>
                      <tr style={{ backgroundColor: '#f8fafc' }}>
                        <th style={{ padding: '8px', textAlign: 'left', borderBottom: '1px solid #e2e8f0' }}>Resource</th>
                        <th style={{ padding: '8px', textAlign: 'right', borderBottom: '1px solid #e2e8f0' }}>Amount</th>
                      </tr>
                    </thead>
                    <tbody>
                      {Object.entries(regionData.storedResources || {}).map(([resource, amount]) => (
                        <tr key={resource} style={{ borderBottom: '1px solid #e2e8f0' }}>
                          <td style={{ padding: '8px', textAlign: 'left' }}>{resource}</td>
                          <td style={{ padding: '8px', textAlign: 'right' }}>{amount}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </div>
            )}

            {activeTab === 'producers' && regionData.producers && (
              <div>
                <h3 style={{ fontSize: '1.25rem', fontWeight: 'bold', marginBottom: '8px' }}>Producers</h3>
                <div style={{
                  display: 'grid',
                  gridTemplateColumns: '1fr 1fr',
                  gap: '16px',
                  marginBottom: '16px'
                }}>
                  {Object.entries(regionData.producers || {}).map(([id, producer]) => (
                    <div
                      key={id}
                      onClick={() => setSelectedProducer(producer)}
                      style={{
                        border: '1px solid #e2e8f0',
                        borderRadius: '4px',
                        padding: '12px',
                        cursor: 'pointer',
                        backgroundColor: selectedProducer?.id === id ? '#eff6ff' : 'white'
                      }}
                    >
                      <div style={{ fontWeight: 'bold', marginBottom: '4px' }}>
                        {producer.type} Producer
                      </div>
                      <div style={{ fontSize: '0.875rem', color: '#4b5563' }}>
                        ID: {id.slice(0, 8)}...
                      </div>
                    </div>
                  ))}
                </div>

                {selectedProducer && (
                  <div style={{
                    border: '1px solid #e2e8f0',
                    borderRadius: '4px',
                    padding: '16px',
                    backgroundColor: '#f8fafc'
                  }}>
                    <h4 style={{ fontSize: '1.125rem', fontWeight: 'bold', marginBottom: '8px' }}>
                      {selectedProducer.type} Producer Details
                    </h4>
                    <pre style={{
                      backgroundColor: 'white',
                      padding: '8px',
                      borderRadius: '4px',
                      overflowX: 'auto',
                      fontSize: '0.875rem'
                    }}>
                      {JSON.stringify(selectedProducer, null, 2)}
                    </pre>
                  </div>
                )}
              </div>
            )}
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
              cursor: 'pointer',
              marginRight: '8px'
            }}
          >
            Refresh
          </button>
          <button
            onClick={onClose}
            style={{
              backgroundColor: '#e5e7eb',
              color: '#374151',
              padding: '8px 16px',
              borderRadius: '4px',
              border: 'none',
              cursor: 'pointer'
            }}
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
};

export default RegionModal;