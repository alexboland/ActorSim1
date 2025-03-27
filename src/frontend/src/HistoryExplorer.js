import React, { useState } from 'react';
import HistoryEventItem from './HistoryEventItem';

const HistoryExplorer = ({ isOpen, onClose }) => {
  const [loading, setLoading] = useState(false);
  const [historyData, setHistoryData] = useState(null);
  const [error, setError] = useState(null);
  const [queryParams, setQueryParams] = useState({
    agentId: '',
    regionId: '',
    eventType: '',
    fromTime: '',
    toTime: '',
    offset: 0,
    limit: 100
  });

  const eventTypes = [
    "", // Empty option
    "ResourceProduced",
    "ResourceConsumed",
    "PopulationChanged",
    "MarketTransaction",
    "BondIssued",
    "BondRepaid",
    "WorkerHired",
    "FounderCreated",
    "ConstructionStarted",
    "ConstructionCompleted",
    "SeasonChanged"
  ];

  const handleInputChange = (e) => {
    const { name, value } = e.target;
    setQueryParams(prev => ({
      ...prev,
      [name]: value
    }));
  };

  const fetchHistory = async () => {
    setLoading(true);
    setError(null);

    try {
      // Build query string from non-empty parameters
      const params = new URLSearchParams();

      Object.entries(queryParams).forEach(([key, value]) => {
        if (value !== '') {
          params.append(key, value);
        }
      });

      const url = `http://localhost:8080/history?${params.toString()}`;
      const response = await fetch(url);

      if (!response.ok) {
        throw new Error(`Failed to fetch history: ${response.status}`);
      }

      const data = await response.json();
      setHistoryData(data);
    } catch (err) {
      console.error('Error fetching history:', err);
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  const fetchRecentHistory = async () => {
    setLoading(true);
    setError(null);

    try {
      const limit = queryParams.limit || 10;
      const url = `http://localhost:8080/history/recent?limit=${limit}`;
      const response = await fetch(url);

      if (!response.ok) {
        throw new Error(`Failed to fetch recent history: ${response.status}`);
      }

      const data = await response.json();
      setHistoryData(data);
    } catch (err) {
      console.error('Error fetching recent history:', err);
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };



  if (!isOpen) return null;

  return (
    <div className="modal-overlay" style={{
      position: 'fixed',
      top: 0,
      left: 0,
      right: 0,
      bottom: 0,
      backgroundColor: 'rgba(0, 0, 0, 0.5)',
      display: 'flex',
      justifyContent: 'center',
      alignItems: 'center',
      zIndex: 1000
    }}>
      <div className="modal-content" style={{
        backgroundColor: 'white',
        padding: '20px',
        borderRadius: '5px',
        width: '80%',
        maxWidth: '800px',
        maxHeight: '90vh',
        overflow: 'auto'
      }}>
        <div className="modal-header" style={{
          display: 'flex',
          justifyContent: 'space-between',
          alignItems: 'center',
          marginBottom: '20px'
        }}>
          <h2>Game History Explorer</h2>
          <button
            onClick={onClose}
            style={{
              background: 'none',
              border: 'none',
              fontSize: '1.5rem',
              cursor: 'pointer'
            }}
          >
            &times;
          </button>
        </div>

        <div className="query-parameters" style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(2, 1fr)',
          gap: '10px',
          marginBottom: '20px'
        }}>
          <div>
            <label htmlFor="agentId">Agent ID:</label>
            <input
              type="text"
              id="agentId"
              name="agentId"
              value={queryParams.agentId}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            />
          </div>

          <div>
            <label htmlFor="regionId">Region ID:</label>
            <input
              type="text"
              id="regionId"
              name="regionId"
              value={queryParams.regionId}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            />
          </div>

          <div>
            <label htmlFor="eventType">Event Type:</label>
            <select
              id="eventType"
              name="eventType"
              value={queryParams.eventType}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            >
              {eventTypes.map(type => (
                <option key={type} value={type}>
                  {type || "All Events"}
                </option>
              ))}
            </select>
          </div>

          <div>
            <label htmlFor="fromTime">From (timestamp):</label>
            <input
              type="number"
              id="fromTime"
              name="fromTime"
              value={queryParams.fromTime}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            />
          </div>

          <div>
            <label htmlFor="toTime">To (timestamp):</label>
            <input
              type="number"
              id="toTime"
              name="toTime"
              value={queryParams.toTime}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            />
          </div>

          <div>
            <label htmlFor="offset">Offset:</label>
            <input
              type="number"
              id="offset"
              name="offset"
              value={queryParams.offset}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            />
          </div>

          <div>
            <label htmlFor="limit">Limit:</label>
            <input
              type="number"
              id="limit"
              name="limit"
              value={queryParams.limit}
              onChange={handleInputChange}
              style={{ width: '100%', padding: '8px', boxSizing: 'border-box' }}
            />
          </div>
        </div>

        <div className="action-buttons" style={{ marginBottom: '20px' }}>
          <button
            onClick={fetchHistory}
            disabled={loading}
            style={{
              padding: '8px 15px',
              marginRight: '10px',
              backgroundColor: '#4CAF50',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: loading ? 'not-allowed' : 'pointer'
            }}
          >
            {loading ? 'Loading...' : 'Fetch History'}
          </button>

          <button
            onClick={fetchRecentHistory}
            disabled={loading}
            style={{
              padding: '8px 15px',
              marginRight: '10px',
              backgroundColor: '#2196F3',
              color: 'white',
              border: 'none',
              borderRadius: '4px',
              cursor: loading ? 'not-allowed' : 'pointer'
            }}
          >
            {loading ? 'Loading...' : 'Recent History'}
          </button>
        </div>

        {error && (
          <div className="error-message" style={{
            color: 'red',
            marginBottom: '15px',
            padding: '10px',
            backgroundColor: '#FFEBEE',
            borderRadius: '4px'
          }}>
            Error: {error}
          </div>
        )}

        {historyData && (
          <div className="history-data">
            <div style={{
              display: 'flex',
              justifyContent: 'space-between',
              alignItems: 'center',
              marginBottom: '15px'
            }}>
              <h3>Results: {historyData.totalCount} events found</h3>
              <div>
                <button
                  onClick={() => {
                    const jsonElement = document.createElement('textarea');
                    jsonElement.value = JSON.stringify(historyData, null, 2);
                    document.body.appendChild(jsonElement);
                    jsonElement.select();
                    document.execCommand('copy');
                    document.body.removeChild(jsonElement);
                    alert('JSON copied to clipboard!');
                  }}
                  style={{
                    padding: '5px 10px',
                    marginRight: '10px',
                    backgroundColor: '#9E9E9E',
                    color: 'white',
                    border: 'none',
                    borderRadius: '4px',
                    cursor: 'pointer'
                  }}
                >
                  Copy as JSON
                </button>
                <button
                  onClick={() => {
                    const jsonElement = document.createElement('a');
                    const blob = new Blob([JSON.stringify(historyData, null, 2)], { type: 'application/json' });
                    jsonElement.href = URL.createObjectURL(blob);
                    jsonElement.download = 'game_history.json';
                    document.body.appendChild(jsonElement);
                    jsonElement.click();
                    document.body.removeChild(jsonElement);
                  }}
                  style={{
                    padding: '5px 10px',
                    backgroundColor: '#FF9800',
                    color: 'white',
                    border: 'none',
                    borderRadius: '4px',
                    cursor: 'pointer'
                  }}
                >
                  Download JSON
                </button>
              </div>
            </div>

            <div className="events-list" style={{
              maxHeight: '500px',
              overflowY: 'auto',
              padding: '10px',
              border: '1px solid #ccc',
              borderRadius: '4px'
            }}>
              {historyData.events.length > 0 ? (
                historyData.events.map((event, index) => (
                  <HistoryEventItem key={`${event.timestamp}-${index}`} event={event} />
                ))
              ) : (
                <div style={{ textAlign: 'center', padding: '20px', color: '#666' }}>
                  No events found matching your criteria.
                </div>
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default HistoryExplorer;